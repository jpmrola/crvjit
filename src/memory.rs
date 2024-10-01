use std::{cell::RefCell, collections::BTreeMap, fs::OpenOptions, rc::Rc};

use memmap2::{MmapMut, MmapOptions};
use std::io::Error;

#[repr(u8)]
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MemoryType {
    Ram,
    Rom,
    Mmio,
    Container,
}

pub trait MemoryOps {
    extern "C" fn read(&self, addr: u64, size: MemoryAccessSize) -> u64;
    extern "C" fn write(&mut self, addr: u64, size: MemoryAccessSize, value: u64);
}

#[repr(C)]
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MemoryAccessSize {
    Byte,
    HalfWord,
    Word,
    DoubleWord,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum MemoryAccessType {
    Read,
    Write,
}

type SubRegionMap = BTreeMap<u64, MemoryRegion>;

#[repr(C)]
pub struct MemoryRegion {
    pub name: String,
    pub base_addr: u64,
    pub size: u64,
    pub data: Option<MmapMut>,
    pub mem_type: MemoryType,
    pub ops: Option<Rc<RefCell<dyn MemoryOps>>>,
    pub subregions: SubRegionMap,
}

impl std::fmt::Debug for MemoryRegion {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt.debug_struct("MemoryRegion")
            .field("name", &self.name)
            .field("base_addr", &self.base_addr)
            .field("size", &self.size)
            .field("subregions", &self.subregions)
            .finish()
    }
}

impl MemoryRegion {

    pub fn new_region(reg_name: &str, addr: u64, reg_size: u64) -> Self {
        MemoryRegion {
            name: reg_name.to_string(),
            base_addr: addr,
            size: reg_size,
            data: None,
            mem_type: MemoryType::Container,
            ops: None,
            subregions: SubRegionMap::new(),
        }
    }

    pub fn new_region_ram(reg_name: &str, addr: u64, ram_size: u64) -> Result<Self, Error> {
        let ram = MmapMut::map_anon(ram_size as usize)?;
        Ok(MemoryRegion {
            name: reg_name.to_string(),
            base_addr: addr,
            size: ram_size,
            data: Some(ram),
            mem_type: MemoryType::Ram,
            ops: None,
            subregions: SubRegionMap::new(),
        })
    }

    pub fn new_region_rom(reg_name: &str, addr: u64, file_path: &str) -> Result<Self, Error> {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .open(file_path)?;

        let rom = unsafe { MmapOptions::new().map(&file)? };
        let rom_size = rom.len() as u64;

        Ok(MemoryRegion {
            name: reg_name.to_string(),
            base_addr: addr,
            size: rom_size,
            data: Some(rom.make_mut()?),
            mem_type: MemoryType::Rom,
            ops: None,
            subregions: SubRegionMap::new(),
        })
    }

    pub fn new_region_mmio(reg_name: &str, addr: u64, mmio_size: u64, ops: Rc<RefCell<dyn MemoryOps>>) -> Self {
        MemoryRegion {
            name: reg_name.to_string(),
            base_addr: addr,
            size: mmio_size,
            data: None,
            mem_type: MemoryType::Mmio,
            ops: Some(ops),
            subregions: SubRegionMap::new(),
        }
    }

    pub fn read(&self, addr: u64, size: MemoryAccessSize) -> u64 {
        if addr >= self.base_addr && addr < self.base_addr + self.size {
            if self.subregions.is_empty() {
                if let Some(handler) = &self.ops{
                    return handler.borrow().read(addr, size);
                } else {
                    panic!("Invalid memory access: no handler, read from 0x{:X}", addr);
                }
            } else {
                panic!("Invalid memory access: not a terminal region, read from 0x{:X}", addr);
            }
        } else {
            panic!("Invalid memory access: out of bounds, read from 0x{:X}", addr);
        }
    }

    pub fn write(&mut self, addr: u64, value: u64, size: MemoryAccessSize) {
        if addr >= self.base_addr && addr < self.base_addr + self.size {
            if self.subregions.is_empty() {
                if let Some(handler) = &mut self.ops {
                    handler.borrow_mut().write(addr, size, value);
                } else {
                    panic!("Invalid memory access: no handler, write from 0x{:X}", addr);
                }
            } else {
                panic!("Invalid memory access: Not a terminal region, write from 0x{:X}", addr);
            }
        } else {
            panic!("Invalid memory access: out of bounds, write from 0x{:X}", addr);
        }
    }

    pub fn read_bytes(&self, addr: u64, size: usize) -> Result<&[u8], Error> {
        if addr < self.base_addr || addr + size as u64 > self.base_addr + self.size {
            return Err(Error::new(std::io::ErrorKind::InvalidInput, "Invalid memory access: out of bounds"))
        }
        let offset = (addr - self.base_addr) as usize;
        let data = self.get_data();
        Ok(&data[offset..(offset + size)])
    }

    pub fn write_bytes(&mut self, addr: u64, data: &[u8]) -> Result<(), Error> {
        if addr < self.base_addr || addr + data.len() as u64 > self.base_addr + self.size {
            return Err(Error::new(std::io::ErrorKind::InvalidInput, "Invalid memory access: out of bounds"))
        }
        let region_ptr = Self::find_subregion_mut(self, addr);
        let region = unsafe { region_ptr.as_mut().expect("Invalid memory region") };
        let offset = (addr - region.base_addr) as usize;
        let mem = region.get_data_mut();
        mem[offset..(offset + data.len())].copy_from_slice(data);
        Ok(())
    }

    pub fn add_subregion(&mut self, subregion: MemoryRegion) {
        let base = subregion.base_addr;
        self.subregions.insert(base, subregion);
    }

    pub extern "C" fn find_subregion(&self, addr: u64) -> *const MemoryRegion {
        if addr >= self.base_addr && addr < self.base_addr + self.size {
            for (_, subregion) in self.subregions.iter() {
                let found = MemoryRegion::find_subregion(subregion, addr);
                if !found.is_null() {
                    return found;
                }
            }
            self
        } else {
            std::ptr::null()
        }
    }

    #[no_mangle]
    pub extern "C" fn find_subregion_mut(this: *mut MemoryRegion, addr: u64) -> *mut MemoryRegion {
        if this.is_null() {
            return std::ptr::null_mut();
        }
        let region = unsafe { &mut *this };
        if addr >= region.base_addr && addr < region.base_addr + region.size {
            for (_, subregion) in region.subregions.iter_mut() {
                let found = MemoryRegion::find_subregion_mut(subregion, addr);
                if !found.is_null() {
                    return found;
                }
            }
            this
        } else {
            std::ptr::null_mut()
        }
    }

    pub fn get_data(&self) -> &MmapMut {
        match self.data {
            Some(ref mem) => { mem }
            None => panic!("No mmmap region allocated for {}", self.name),
        }
    }

    pub fn get_data_mut(&mut self) -> &mut MmapMut {
        match self.data {
            Some(ref mut mem) => { mem }
            None => panic!("No mmmap region allocated for {}", self.name),
        }
    }
}

