use cranelift::jit::JITModule;
use cranelift::prelude::*;
use memoffset::offset_of;

use crate::jit::{generate_find_subregion_mut, generate_get_data_mut, generate_mmio_read, generate_mmio_write, Exception, FunctionTranslator};
use crate::memory::{MemoryAccessSize, MemoryAccessType, MemoryRegion, MemoryType};

use crate::riscv::instruction::{decode, get_handler, RiscvInstructionFields};
use crate::riscv::csr::{MARCHID, MHARTID, MIDELEG, MIE, MIMPID, MIP, MSTATUS, MVENDORID, SSTATUS, SSTATUS_MASK};
use crate::riscv::riscv_cpu::RiscvCpuState;

use super::csr::{SIE, SIP};
use super::trap::RiscvTrap;

const INST_SIZE: usize = 4;

pub enum IntegerSign {
    Signed,
    Unsigned,
    DontCare,
}

pub struct TranslationContext<'a,'b> {
    pub pc: u64,
    pub inst: &'a RiscvInstructionFields,
    pub mem: &'a mut MemoryRegion,
    pub module: &'a mut JITModule,
    pub builder: &'a mut FunctionBuilder<'b>,
}

impl<'a,'b> TranslationContext<'a,'b> {
    pub fn new(pc: u64, inst: &'a RiscvInstructionFields, mem: &'a mut MemoryRegion, module: &'a mut JITModule, builder: &'a mut FunctionBuilder<'b>) -> Self {
        TranslationContext {
            pc,
            inst,
            mem,
            module,
            builder,
        }
    }
}

pub struct RiscvTranslator {
    state: Box<RiscvCpuState>,
}

impl RiscvTranslator {
    pub fn new(state: RiscvCpuState) -> Self {
        RiscvTranslator {
            state: Box::new(state),
        }
    }

    pub fn translate_addr(&self, addr: Value) -> Value {
        addr
    }

    fn generate_sload(ctx: &mut TranslationContext, addr: Value, size: MemoryAccessSize, offset: i32) -> Value {
        match size {
            MemoryAccessSize::Byte => ctx.builder.ins().sload8(types::I64, MemFlags::new(), addr, offset),
            MemoryAccessSize::HalfWord => ctx.builder.ins().sload16(types::I64, MemFlags::new(), addr, offset),
            MemoryAccessSize::Word => ctx.builder.ins().sload32(MemFlags::new(), addr, offset),
            MemoryAccessSize::DoubleWord => ctx.builder.ins().load(types::I64, MemFlags::new(), addr, offset),
        }
    }

    fn generate_uload(ctx: &mut TranslationContext, addr: Value, size: MemoryAccessSize, offset: i32) -> Value {
        match size {
            MemoryAccessSize::Byte => ctx.builder.ins().uload8(types::I64, MemFlags::new(), addr, offset),
            MemoryAccessSize::HalfWord => ctx.builder.ins().uload16(types::I64, MemFlags::new(), addr, offset),
            MemoryAccessSize::Word => ctx.builder.ins().uload32(MemFlags::new(), addr, offset),
            MemoryAccessSize::DoubleWord => ctx.builder.ins().load(types::I64, MemFlags::new(), addr, offset),
        }
    }

    fn generate_store(ctx: &mut TranslationContext, addr: Value, val: Value, size: MemoryAccessSize, offset: i32) {
        match size {
            MemoryAccessSize::Byte => ctx.builder.ins().istore8(MemFlags::new(), val, addr, offset),
            MemoryAccessSize::HalfWord => ctx.builder.ins().istore16(MemFlags::new(), val, addr, offset),
            MemoryAccessSize::Word => ctx.builder.ins().istore32(MemFlags::new(), val, addr, offset),
            MemoryAccessSize::DoubleWord => ctx.builder.ins().store(MemFlags::new(), val, addr, offset),
        };
    }

    pub fn csr_read(&self, ctx: &mut TranslationContext, csr: u64) -> Value {
        let csrs_ptr = ctx.builder.ins().iconst(types::I64, self.state.csr.as_ptr() as i64);
        match csr {
            SSTATUS => {
                let mstatus = ctx.builder.ins().load(types::I64, MemFlags::new(), csrs_ptr, MSTATUS as i32 * 8);
                ctx.builder.ins().band_imm(mstatus, SSTATUS_MASK as i64)
            },
            SIE => {
                let mie = ctx.builder.ins().load(types::I64, MemFlags::new(), csrs_ptr, MIE as i32 * 8);
                let mideleg = ctx.builder.ins().load(types::I64, MemFlags::new(), csrs_ptr, MIDELEG as i32 * 8);
                ctx.builder.ins().band(mie, mideleg)
            },
            SIP => {
                let mip = ctx.builder.ins().load(types::I64, MemFlags::new(), csrs_ptr, MIP as i32 * 8);
                let mideleg = ctx.builder.ins().load(types::I64, MemFlags::new(), csrs_ptr, MIDELEG as i32 * 8);
                ctx.builder.ins().band(mip, mideleg)
            },
            _ => {
                ctx.builder.ins().load(types::I64, MemFlags::new(), csrs_ptr, csr as i32 * 8)
            }
        }
    }

    pub fn csr_write(&mut self, ctx: &mut TranslationContext, csr: u64, val: Value) {
        let csrs_ptr = ctx.builder.ins().iconst(types::I64, self.state.csr.as_mut_ptr() as i64);
        match csr {
            MVENDORID => {
                return;
            },
            MARCHID => {
                return;
            },
            MIMPID => {
                return;
            },
            MHARTID => {
                return;
            },
            SSTATUS => {
                let sstatus = ctx.builder.ins().band_imm(val, SSTATUS_MASK as i64);
                ctx.builder.ins().store(MemFlags::new(), sstatus, csrs_ptr, MSTATUS as i32 * 8);
                return;
            },
            SIE => {
                let mideleg = ctx.builder.ins().load(types::I64, MemFlags::new(), csrs_ptr, MIDELEG as i32 * 8);
                let sie = ctx.builder.ins().band(val, mideleg);
                ctx.builder.ins().store(MemFlags::new(), sie, csrs_ptr, MIE as i32 * 8);
                return;
            },
            SIP => {
                let mideleg = ctx.builder.ins().load(types::I64, MemFlags::new(), csrs_ptr, MIDELEG as i32 * 8);
                let sip = ctx.builder.ins().band(val, mideleg);
                ctx.builder.ins().store(MemFlags::new(), sip, csrs_ptr, MIP as i32 * 8);
                return;
            },
            _ => {
                ctx.builder.ins().store(MemFlags::new(), val, csrs_ptr, csr as i32 * 8);
            },
        }
    }

    pub fn reg_read(&self, ctx: &mut TranslationContext, reg: u64) -> Value {
        let regs_ptr = ctx.builder.ins().iconst(types::I64, self.state.regs.as_ptr() as i64);
        ctx.builder.ins().load(types::I64, MemFlags::new(), regs_ptr, reg as i32 * 8)
    }

    pub fn reg_write(&mut self, ctx: &mut TranslationContext, reg: u64, val: Value) {
        if reg != 0 {
            let regs_ptr = ctx.builder.ins().iconst(types::I64, self.state.regs.as_mut_ptr() as i64);
            ctx.builder.ins().store(MemFlags::new(), val, regs_ptr, reg as i32 * 8);
        }
    }

    fn generate_memory_access(&self, ctx: &mut TranslationContext, addr: Value, size: MemoryAccessSize, access_type: MemoryAccessType, sign: IntegerSign, val: Option<Value>) -> Option<Value> {
        let phy_addr = self.translate_addr(addr);
        let region = generate_find_subregion_mut(ctx.module, ctx.builder, phy_addr, ctx.mem);
        let region_base_addr = ctx.builder.ins().load(types::I64, MemFlags::new(), region, offset_of!(MemoryRegion, base_addr) as i32);
        let region_type = ctx.builder.ins().load(types::I8, MemFlags::new(), region, offset_of!(MemoryRegion, mem_type) as i32);

        let is_ram = ctx.builder.ins().icmp_imm(IntCC::Equal, region_type, MemoryType::Ram as i64);
        let is_rom = ctx.builder.ins().icmp_imm(IntCC::Equal, region_type, MemoryType::Rom as i64);
        let is_mem = ctx.builder.ins().bor(is_ram, is_rom);

        let mem_block = ctx.builder.create_block();
        let mmio_block = ctx.builder.create_block();
        let merge_block = ctx.builder.create_block();
        if access_type == MemoryAccessType::Read {
            ctx.builder.append_block_param(merge_block, types::I64);
        }

        ctx.builder.ins().brif(is_mem, mem_block, &[], mmio_block, &[]);

        ctx.builder.switch_to_block(mem_block);
        ctx.builder.seal_block(mem_block);
        {
            let rel_addr = ctx.builder.ins().isub(phy_addr, region_base_addr);
            let data = generate_get_data_mut(ctx.module, ctx.builder, region);
            let native_addr = ctx.builder.ins().iadd(rel_addr, data);

            match access_type {
                MemoryAccessType::Read => {
                    let out_val: Value;
                    match sign {
                        IntegerSign::Signed => {
                            out_val = RiscvTranslator::generate_sload(ctx, native_addr, size, 0);
                        },
                        _ => {
                            out_val = RiscvTranslator::generate_uload(ctx, native_addr, size, 0);
                        },
                    };
                    ctx.builder.ins().jump(merge_block, &[out_val]);
                },
                MemoryAccessType::Write => {
                    RiscvTranslator::generate_store(ctx, native_addr, val.unwrap(), size, 0);
                    ctx.builder.ins().jump(merge_block, &[]);
                },

            }
        }

        ctx.builder.switch_to_block(mmio_block);
        ctx.builder.seal_block(mmio_block);
        {
            match access_type {
                MemoryAccessType::Read => {
                    let out_val = generate_mmio_read(ctx.module, ctx.builder, phy_addr, size, region);
                    ctx.builder.ins().jump(merge_block, &[out_val]);
                },
                MemoryAccessType::Write => {
                    generate_mmio_write(ctx.module, ctx.builder, phy_addr, size, val.unwrap(), region);
                    ctx.builder.ins().jump(merge_block, &[]);
                },
            };
        }


        ctx.builder.switch_to_block(merge_block);
        ctx.builder.seal_block(merge_block);

        if access_type == MemoryAccessType::Read {
            Some(ctx.builder.block_params(merge_block)[0])
        } else {
            None
        }
    }

    pub fn generate_memory_read(&self, ctx: &mut TranslationContext, addr: Value, size: MemoryAccessSize, sign: IntegerSign) -> Value {
        self.generate_memory_access(ctx, addr, size, MemoryAccessType::Read, sign, None).unwrap()
    }

    pub fn generate_memory_write(&self, ctx: &mut TranslationContext, addr: Value, size: MemoryAccessSize, val: Value) {
        self.generate_memory_access(ctx, addr, size, MemoryAccessType::Write, IntegerSign::DontCare, Some(val));
    }
}

impl FunctionTranslator for RiscvTranslator {

    fn get_pc(&self) -> u64 {
        self.state.pc
    }

    fn set_pc(&mut self, addr: u64) {
        self.state.pc = addr
    }

    fn get_inst_size(&self) -> usize {
        INST_SIZE
    }

    fn fetch_instruction(&self, addr: u64, mem: &mut MemoryRegion) -> u64 {
        let ram_ptr = MemoryRegion::find_subregion(mem, addr);
        if !ram_ptr.is_null(){
            let ram = unsafe { & *ram_ptr };
            let index = (addr - ram.base_addr) as usize;
            let data = ram.get_data();
            u32::from_le_bytes(data[index .. index + 4].try_into().expect("slice incorrect length")) as u64
        } else {
            panic!("Can't fetch instruction at {:#X?}", addr)
        }
    }

    fn translate_instruction(&mut self, module: &mut JITModule, builder: &mut FunctionBuilder, inst: u64, pc: u64, mem: &mut MemoryRegion) -> Result<Option<Value>, Exception> {
        let inst = &decode(inst).map_err(|mut e| {
            e.val = pc;
            Exception::Riscv(e)
        })?;
        let inst_handler = get_handler(inst).map_err(|mut e| {
            e.val = pc;
            Exception::Riscv(e)
        })?;

        let mut ctx = TranslationContext::new(pc, inst, mem, module, builder);

        (inst_handler.translate)(self, &mut ctx).map_err(|mut e| {
            e.val = pc;
            Exception::Riscv(e)
        })
    }

    fn handle_trap(&mut self, trap: RiscvTrap) -> Result<(), Exception> {
        // TODO: Implement trap handling
        // Create a new block for the trap handler
        // Call an extern C function to handle the trap
        Ok(())
    }
}

