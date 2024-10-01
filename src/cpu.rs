use crate::memory::MemoryRegion;

pub trait Cpu {
    fn step(&mut self, mem: &mut MemoryRegion);
    fn run(&mut self, mem: &mut MemoryRegion);
    fn debug_info(&self) -> String;
}

impl std::fmt::Debug for dyn Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.debug_info())
    }
}

#[derive(Debug)]
pub struct CpuInfo {
    pub arch: Arch,
    pub num_harts: u32,
    // TODO: CpuCaps
}

#[derive(Debug)]
pub enum Arch {
    RiscV,
}

