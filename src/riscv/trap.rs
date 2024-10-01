use crate::bit;

#[allow(dead_code)]
pub enum RiscvTrapCause {
    // Exceptions
    InstructionAddressMisaligned,
    InstructionAccessFault,
    IllegalInstruction,
    Breakpoint,
    LoadAddressMisaligned,
    LoadAccessFault,
    StoreAMOAddressMisaligned,
    StoreAMOAccessFault,
    EnvironmentCallFromUMode,
    EnvironmentCallFromSMode,
    InstructionPageFault,
    LoadPageFault,
    StoreAMOPageFault,
    // Interrupts
    SupervisorSoftwareInterrupt,
    MachineSoftwareInterrupt,
    SupervisorTimerInterrupt,
    MachineTimerInterrupt,
    SupervisorExternalInterrupt,
    MachineExternalInterrupt,
}

pub struct RiscvTrap {
    pub cause: RiscvTrapCause,
    pub val: u64,
}

impl RiscvTrap {
    pub fn new(cause: RiscvTrapCause, val: u64) -> RiscvTrap {
        RiscvTrap { cause, val }
    }
}

impl RiscvTrap {

    pub fn get_trap_code(&self) -> u64 {
        match self.cause {
            RiscvTrapCause::InstructionAddressMisaligned => 0,
            RiscvTrapCause::InstructionAccessFault => 1,
            RiscvTrapCause::IllegalInstruction => 2,
            RiscvTrapCause::Breakpoint => 3,
            RiscvTrapCause::LoadAddressMisaligned => 4,
            RiscvTrapCause::LoadAccessFault => 5,
            RiscvTrapCause::StoreAMOAddressMisaligned => 6,
            RiscvTrapCause::StoreAMOAccessFault => 7,
            RiscvTrapCause::EnvironmentCallFromUMode => 8,
            RiscvTrapCause::EnvironmentCallFromSMode => 9,
            RiscvTrapCause::InstructionPageFault => 12,
            RiscvTrapCause::LoadPageFault => 13,
            RiscvTrapCause::StoreAMOPageFault => 15,
            RiscvTrapCause::SupervisorSoftwareInterrupt => 1 | bit!(63),
            RiscvTrapCause::MachineSoftwareInterrupt => 3 | bit!(63),
            RiscvTrapCause::SupervisorTimerInterrupt => 5 | bit!(63),
            RiscvTrapCause::MachineTimerInterrupt => 7 | bit!(63),
            RiscvTrapCause::SupervisorExternalInterrupt => 9 | bit!(63),
            RiscvTrapCause::MachineExternalInterrupt => 11 | bit!(63),
        }
    }

    pub fn get_trap_str(&self) -> &'static str {
        match self.cause {
            RiscvTrapCause::InstructionAddressMisaligned => "InstructionAddressMisaligned",
            RiscvTrapCause::InstructionAccessFault => "InstructionAccessFault",
            RiscvTrapCause::IllegalInstruction => "IllegalInstruction",
            RiscvTrapCause::Breakpoint => "Breakpoint",
            RiscvTrapCause::LoadAddressMisaligned => "LoadAddressMisaligned",
            RiscvTrapCause::LoadAccessFault => "LoadAccessFault",
            RiscvTrapCause::StoreAMOAddressMisaligned => "StoreAMOAddressMisaligned",
            RiscvTrapCause::StoreAMOAccessFault => "StoreAMOAccessFault",
            RiscvTrapCause::EnvironmentCallFromUMode => "EnvironmentCallFromUMode",
            RiscvTrapCause::EnvironmentCallFromSMode => "EnvironmentCallFromSMode",
            RiscvTrapCause::InstructionPageFault => "InstructionPageFault",
            RiscvTrapCause::LoadPageFault => "LoadPageFault",
            RiscvTrapCause::StoreAMOPageFault => "StoreAMOPageFault",
            RiscvTrapCause::SupervisorSoftwareInterrupt => "SupervisorSoftwareInterrupt",
            RiscvTrapCause::MachineSoftwareInterrupt => "MachineSoftwareInterrupt",
            RiscvTrapCause::SupervisorTimerInterrupt => "SupervisorTimerInterrupt",
            RiscvTrapCause::MachineTimerInterrupt => "MachineTimerInterrupt",
            RiscvTrapCause::SupervisorExternalInterrupt => "SupervisorExternalInterrupt",
            RiscvTrapCause::MachineExternalInterrupt => "MachineExternalInterrupt",
        }
    }
}
