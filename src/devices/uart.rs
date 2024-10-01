use std::{cell::RefCell, rc::Rc};
use log::info;

use crate::{device::DeviceOps, memory::{MemoryAccessSize, MemoryOps}};

#[derive(Debug)]
pub struct UartState {
    data: u8,
}

#[derive(Debug)]
pub struct UartDevice {
    name: String,
    pub state: Rc<RefCell<UartState>>,
}

impl UartDevice {
    pub fn new(dev_name: &str) -> UartDevice {
        let state = Rc::new(RefCell::new(UartState {
            data: 0,
        }));

        let dev = UartDevice {
            name: dev_name.to_string(),
            state,
        };

        info!("UartDevice {} created", dev_name);

        dev
    }
}

impl MemoryOps for UartState {
    extern "C" fn read(&self, addr: u64, size: MemoryAccessSize) -> u64 {
        self.data as  u64
    }

    extern "C" fn write(&mut self, addr: u64, size: MemoryAccessSize, value: u64) {
        self.data = value as u8
    }

}

impl DeviceOps for UartDevice {
    fn init(&mut self) {
        info!("UartDevice {} initialized", self.name);
    }

    fn shutdown(&mut self) {
        todo!()
    }

    fn reset(&mut self) {
        todo!()
    }

    fn debug_info(&self) -> String {
        format!("{:#X?}", self)
    }
}

