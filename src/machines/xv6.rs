use std::{io::Read, rc::Rc};
use log::info;

use crate::{device::Bus, devices::uart::UartDevice, machine::*, memory::MemoryRegion, riscv::riscv_cpu::RiscvCpu};

pub const RAM_BASE: u64 = 0x80000000;
pub const RAM_END: u64 = 0x88000000;
pub const PLIC_BASE: u64 = 0x0c000000;
pub const PLIC_END: u64 = 0x0c000000 + 0x208000;
pub const UART_BASE: u64 = 0x10000000;
pub const UART_END: u64 = 0x10000000 + 0x100;
pub const DISK_BASE: u64 = 0x10001000;
pub const DISK_END: u64 = 0x10001000 + 0x1000;
pub const CLINT_BASE: u64 = 0x02000000;
pub const CLINT_END: u64 = 0x02000000 + 0x10000;
pub const ROM_BASE: u64 = 0x1000;
pub const ROM_END: u64 = 0x1000 + 0xf000;

#[derive(Debug)]
pub struct Xv6Machine {
    name: String,
    machine_state: MachineState,
}

impl Xv6Machine {
    pub fn new()  -> Self {
        let sys_mem = MemoryRegion::new_region("system-memory", u64::MIN, u64::MAX);
        Xv6Machine {
            name: "xv6-machine".to_string(),
            machine_state: MachineState {
                cpu: Box::new(RiscvCpu::new(RAM_BASE)),
                mem_map: sys_mem,
                main_bus: Bus::new("main-bus"),
            },
        }
    }
}

impl MachineOps for Xv6Machine {
    fn init(&mut self) {
        info!("{} machine init", self.name);
        // RAM
        match self.machine_state.add_ram_region("ram", RAM_BASE, RAM_END - RAM_BASE) {
            Err(_) => panic!("Memory allocation failed"),
            Ok(_) => {},
        }

        // UART
        let uart0: Box<UartDevice> = Box::new(UartDevice::new("uart0"));
        let ops = Rc::clone(&uart0.state);
        match self.machine_state.add_mmio_region("uart0", UART_BASE, UART_END - UART_BASE, ops) {
            Err(_) => panic!("Memory allocation failed"),
            Ok(_) => {},
        }
        self.machine_state.add_device(uart0);

        // INIT
        self.machine_state.main_bus.init();
    }

    fn run(&mut self) {
        info!("{} machine run", self.name);
        self.machine_state.cpu.run(&mut self.machine_state.mem_map);
    }

    fn reset(&mut self) {
        todo!()
    }

    fn get_name(&self) -> String {
        format!("{}", self.name)
    }

    fn debug_info(&self) -> String {
        format!("Machine: {}\n{:#X?}", self.name, self.machine_state)
    }

    fn load_bin(&mut self, bin_path: &str) -> Result<(), std::io::Error> {
        let mut bin_file = std::fs::File::open(bin_path)?;
        let mut data = Vec::new();
        bin_file.read_to_end(&mut data)?;
        if let Err(e) = self.machine_state.mem_map.write_bytes(RAM_BASE, &data) {
            return Err(e);
        }
        info!("Binary file {} loaded at 0x{:X}", bin_path, RAM_BASE);
        Ok(())
    }
}
