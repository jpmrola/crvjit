use log::debug;

use cranelift::prelude::*;
use cranelift::jit::{JITBuilder, JITModule};
use cranelift::module::{Linkage, Module};
use rustc_hash::FxHashMap;

use crate::memory::{MemoryAccessSize, MemoryRegion};
use crate::riscv::trap::RiscvTrap;

const MAX_TB_SIZE: usize = 128;

struct TranslationBlock {
    code: *const u8,
    size: usize,
}

pub enum Exception {
    MemFault,
    CodegenFault(String),
    // TODO: Make ISA traps enum
    Riscv(RiscvTrap),
}

type TranslationCache = FxHashMap<u64, TranslationBlock>;

pub trait FunctionTranslator {
    fn get_pc(&self) -> u64;
    fn set_pc(&mut self, addr: u64);
    fn get_inst_size(&self) -> usize;
    fn fetch_instruction(&self, addr: u64, mem: &mut MemoryRegion) -> u64;
    fn translate_instruction(&mut self, module: &mut JITModule, builder: &mut FunctionBuilder, inst: u64 , pc: u64, mem: &mut MemoryRegion) -> Result<Option<Value>, Exception>;
    fn handle_trap(&mut self, trap: RiscvTrap) -> Result<(), Exception>;
}

pub struct JIT<T: FunctionTranslator> {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
    tb_cache: TranslationCache,
    translator: T,
    exception: Option<Exception>,
}

impl<T: FunctionTranslator> JIT<T> {
    pub fn new(translator: T) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift::native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift::module::default_libcall_names());
        builder.symbol("get_data_mut", get_data_mut as *const u8);
        builder.symbol("find_subregion_mut", MemoryRegion::find_subregion_mut as *const u8);
        builder.symbol("mmio_read", mmio_read as *const u8);
        builder.symbol("mmio_write", mmio_write as *const u8);

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            tb_cache: TranslationCache::default(),
            translator,
            exception: None,
        }
    }

    pub fn translate_block(&mut self, pc: u64, mem: &mut MemoryRegion) -> Result<*const u8, Exception> {
        if let Some(tb) = self.tb_cache.get(&pc) {
            debug!("TB cache hit: 0x{:X}", pc);
            return Ok(tb.code);
        }

        debug!("TB cache miss: 0x{:X}", pc);

        self.ctx.func.signature = self.module.make_signature();
        self.ctx.func.signature.returns.push(AbiParam::new(types::I64));
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut current_pc = pc;
        let mut inst_count = 0;
        let mut pc_jump: Option<Value> = None;
        while inst_count < MAX_TB_SIZE {
            let inst = self.translator.fetch_instruction(current_pc, mem);
            let res = self.translator.translate_instruction(&mut self.module, &mut builder, inst, current_pc, mem);
            match res {
                Ok(Some(next_pc)) => {
                    pc_jump = Some(next_pc);
                    break;
                },
                Err(e) => {
                    self.exception = Some(e);
                    break;
                },
                Ok(None) => (),
            }
            current_pc += self.translator.get_inst_size() as u64;
            inst_count += 1;
        }

        if let Some(pc_jump) = pc_jump {
            builder.ins().return_(&[pc_jump]);
        } else {
            let next_pc = builder.ins().iconst(types::I64, current_pc as i64);
            builder.ins().return_(&[next_pc]);
        }
        builder.finalize();

        let id = self.module
            .declare_anonymous_function(&self.ctx.func.signature)
            .map_err(|e| Exception::CodegenFault(e.to_string()))?;

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| Exception::CodegenFault(e.to_string()))?;

        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();

        let code = self.module.get_finalized_function(id);

        let tb = TranslationBlock {
            code,
            size: inst_count * self.translator.get_inst_size(),
        };

        self.tb_cache.insert(pc, tb);

        debug!("TB added to cache: 0x{:X}", pc);

        Ok(code)
    }

    pub fn handle_isa_trap(&mut self) -> Result<(), Exception> {
        if let Some(e) = self.exception.take() {
            match e {
                Exception::Riscv(trap) => {
                    self.translator.handle_trap(trap)?;
                },
                _ => return Err(e),
            }
        }
        Ok(())
    }

    pub fn get_pc(&self) -> u64 {
        self.translator.get_pc()
    }

    pub fn set_pc(&mut self, addr: u64) {
        self.translator.set_pc(addr)
    }
}

#[no_mangle]
pub extern "C" fn get_data_mut(region: *mut MemoryRegion) -> *mut u8 {
    let region = unsafe { &mut *region };
    region.get_data_mut().as_mut_ptr()
}

#[no_mangle]
pub extern "C" fn mmio_read(addr: u64, size: MemoryAccessSize, region: *mut MemoryRegion) -> u64 {
    let region = unsafe { &mut *region };
    region.read(addr, size)
}

#[no_mangle]
pub extern "C" fn mmio_write(addr: u64, size: MemoryAccessSize, value: u64, region: *mut MemoryRegion) {
    let region = unsafe { &mut *region };
    region.write(addr, value, size);
}

pub fn generate_find_subregion_mut(module: &mut JITModule, builder: &mut FunctionBuilder, addr: Value, region: *mut MemoryRegion) -> Value {
    let mut sig = module.make_signature();

    // Params: addr, region ptr
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));

    // Returns: region ptr
    sig.returns.push(AbiParam::new(types::I64));

    let callee = module
        .declare_function("find_subregion_mut", Linkage::Import, &sig)
        .expect("problem declaring function");

    let local_callee = module.declare_func_in_func(callee, builder.func);
    let mut arg_values = Vec::new();
    arg_values.push(builder.ins().iconst(types::I64, region as i64));
    arg_values.push(addr);
    let call = builder.ins().call(local_callee, &arg_values);
    builder.inst_results(call)[0]
}

pub fn generate_get_data_mut(module: &mut JITModule, builder: &mut FunctionBuilder, region: Value) -> Value {
    let mut sig = module.make_signature();

    // Params: region ptr
    sig.params.push(AbiParam::new(types::I64));

    // Returns: data ptr
    sig.returns.push(AbiParam::new(types::I64));

    let callee = module
        .declare_function("get_data_mut", Linkage::Import, &sig)
        .expect("problem declaring function");

    let local_callee = module.declare_func_in_func(callee, builder.func);
    let mut arg_values = Vec::new();
    arg_values.push(region);
    let call = builder.ins().call(local_callee, &arg_values);
    builder.inst_results(call)[0]
}

pub fn generate_mmio_read(module: &mut JITModule, builder: &mut FunctionBuilder,
addr: Value, size: MemoryAccessSize, region: Value) -> Value {
    // MMIO READ generator
    let mut sig = module.make_signature();

    // Params: addr, size, region ptr
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));

    // Returns: value
    sig.returns.push(AbiParam::new(types::I64));

    let callee = module
        .declare_function("mmio_read", Linkage::Import, &sig)
        .expect("problem declaring function");

    let local_callee = module.declare_func_in_func(callee, builder.func);
    let mut arg_values = Vec::new();
    arg_values.push(addr);
    arg_values.push(builder.ins().iconst(types::I64, size as i64));
    arg_values.push(region);
    let call = builder.ins().call(local_callee, &arg_values);
    builder.inst_results(call)[0]
}

pub fn generate_mmio_write(module: &mut JITModule, builder: &mut FunctionBuilder,
addr: Value, size: MemoryAccessSize, value: Value, region: Value) {
    // MMIO WRITE generator
    let mut sig = module.make_signature();

    // Params: addr, size, value, region ptr
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));
    sig.params.push(AbiParam::new(types::I64));

    // Returns: void
    sig.returns.clear();

    let callee = module
        .declare_function("write", Linkage::Import, &sig)
        .expect("problem declaring function");

    let local_callee = module.declare_func_in_func(callee, builder.func);
    let mut arg_values = Vec::new();
    arg_values.push(addr);
    arg_values.push(builder.ins().iconst(types::I64, size as i64));
    arg_values.push(value);
    arg_values.push(region);
    builder.ins().call(local_callee, &arg_values);
}

