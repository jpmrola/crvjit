use std::env::args;
use log::error;
use log::info;

use machine::MachineOps;
use machines::xv6::Xv6Machine;

mod machine;
mod machines;
mod riscv;
mod memory;
mod device;
mod devices;
mod cpu;
mod jit;

fn main() {
    env_logger::init();
    let args: Vec<String> = args().collect();

    if args.len() != 2 {
        error!("Invalid arguments");
        println!("Usage: {} <bin>", args[0]);
        return;
    }

    let mut machine: Box<dyn MachineOps> = Box::new(Xv6Machine::new());

    machine.init();
    machine.load_bin(&args[1]).unwrap();
    info!("Machine {} ready\n{:?}", machine.get_name(), machine);
    machine.run();
}

