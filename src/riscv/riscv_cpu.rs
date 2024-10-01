use std::{thread::sleep, time::Duration};
use log::error;
use log::debug;

use crate::{cpu::{Arch, Cpu, CpuInfo}, jit::{JIT, Exception}, memory::MemoryRegion};

use super::translator::RiscvTranslator;

const N_REGS: usize = 32;
const N_CSR: usize = 4096;

#[repr(C)]
#[derive(Debug)]
pub struct RiscvCpuState {
    pub pc: u64,
    pub regs: [u64;N_REGS],
    pub csr: [u64;N_CSR],
}

impl RiscvCpuState {
    pub fn new(pc: u64) -> Self {
        RiscvCpuState {
            pc,
            regs: [0;N_REGS],
            csr: [0;N_CSR],
        }
    }
}

pub struct RiscvCpu {
    pub topology: CpuInfo,
    pub jit: JIT<RiscvTranslator>,
}

impl RiscvCpu {

    pub fn new(pc: u64) -> Self {
        let state = RiscvCpuState::new(pc);
        RiscvCpu {
            topology: CpuInfo { arch: Arch::RiscV, num_harts: 1},
            jit: JIT::new(RiscvTranslator::new(state)),
        }
    }

    pub fn execute(&mut self, start_pc: u64, mem: &mut MemoryRegion) -> Result<u64, String> {
        let mut current_pc = start_pc;

        let code_ptr = self.jit.translate_block(current_pc, mem).map_err(|e|
            match e {
                Exception::MemFault => "Memory fault".to_string(),
                Exception::CodegenFault(msg) => format!("Codegen fault: {}", msg),
                Exception::Riscv(e) => e.get_trap_str().to_string(),
            }
        )?;
        let tb_func: extern "C" fn() -> u64 = unsafe { std::mem::transmute::<_,_>(code_ptr) };
        current_pc = tb_func();

        self.jit.handle_isa_trap().map_err(|e|
            match e {
                Exception::MemFault => "Memory fault".to_string(),
                Exception::CodegenFault(msg) => format!("Codegen fault: {}", msg),
                Exception::Riscv(e) => e.get_trap_str().to_string(),
            }
        )?;

        Ok(current_pc)
    }

    pub fn check_interrupts(&self) -> bool { false }
}

impl Cpu for RiscvCpu {
    fn step(&mut self, mem: &mut MemoryRegion) {
        debug!("CPU step at PC: 0x{:X}", self.jit.get_pc());
        match self.execute(self.jit.get_pc(), mem) {
            Ok(new_pc) => self.jit.set_pc(new_pc),
            Err(e) => error!("Error: {}", e),
        }
    }

    fn run(&mut self, mem: &mut MemoryRegion) {
        loop {
            sleep(Duration::from_millis(500));
            self.step(mem);
            if self.check_interrupts() {
                break;
            }
        }
    }

    fn debug_info(&self) -> String {
        format!("{:#?}", self.topology)
    }

}

