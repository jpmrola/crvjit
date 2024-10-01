use std::{cell::RefCell, io::Error, rc::Rc};
use log::info;

use crate::{cpu::Cpu, device::{Bus, DeviceOps}, memory::{MemoryOps, MemoryRegion}};

pub trait MachineOps {
    fn init(&mut self);
    fn run(&mut self);
    fn load_bin(&mut self, bin_path: &str) -> Result<(), Error>;
    fn reset(&mut self);
    fn get_name(&self) -> String;
    fn debug_info(&self) -> String;
}

impl std::fmt::Debug for dyn MachineOps {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.debug_info())
    }
}

#[derive(Debug)]
pub struct MachineState {
    pub cpu: Box<dyn Cpu>,
    pub mem_map: MemoryRegion,
    pub main_bus: Bus,
}

impl MachineState {

    pub fn add_ram_region(&mut self, name: &str, base_addr: u64, size: u64) -> Result<(), Error> {
        let ram = MemoryRegion::new_region_ram(name, base_addr, size)?;
        info!("RAM region added: {:#X?}", ram);
        Ok(self.mem_map.add_subregion(ram))
    }

    pub fn add_mmio_region(&mut self, name: &str, base_addr: u64, size: u64, ops: Rc<RefCell<dyn MemoryOps>>) -> Result<(), Error> {
        let mmio = MemoryRegion::new_region_mmio(name, base_addr, size, ops);
        info!("MMIO region added: {:#X?}", mmio);
        Ok(self.mem_map.add_subregion(mmio))
    }

    pub fn add_device(&mut self, device: Box<dyn DeviceOps>) {
        info!("Device added to bus: {}", device.debug_info());
        self.main_bus.add_to_bus(device);
    }
}

