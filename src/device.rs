pub trait DeviceOps {
    fn init(&mut self);
    fn shutdown(&mut self);
    fn reset(&mut self);
    fn debug_info(&self) -> String;
}

impl std::fmt::Debug for dyn DeviceOps {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.debug_info())
    }
}

pub struct Bus {
    name: String,
    devices: Vec<Box<dyn DeviceOps>>,
}

impl std::fmt::Debug for Bus {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // write the name of the bus and print the devices with debug info
        for dev in self.devices.iter() {
            write!(f, "{}", dev.debug_info())?;
        }
        write!(f, "")
    }
}

impl Bus {
    pub fn new(name: &str) -> Self {
        Bus {
            name: name.to_string(),
            devices: Vec::new(),
        }
    }

     pub fn add_to_bus(&mut self, dev: Box<dyn DeviceOps>) {
        self.devices.push(dev);
    }

    pub fn get_device(&self, name: &str) -> Option<&Box<dyn DeviceOps>> {
        for dev in self.devices.iter() {
            if dev.debug_info() == name {
                return Some(dev);
            }
        }
        None
    }

    pub fn get_device_mut(&mut self, name: &str) -> Option<&mut Box<dyn DeviceOps>> {
        for dev in self.devices.iter_mut() {
            if dev.debug_info() == name {
                return Some(dev);
            }
        }
        None
    }

    pub fn init(&mut self) {
        for dev in self.devices.iter_mut() {
            dev.init();
        }
    }
}

