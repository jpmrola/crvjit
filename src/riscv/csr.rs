use crate::bit;

use super::instruction::bitmask;

// ------------------------------------------------
// Unprivilefed CSR
const CYCLE: u64 = 0xc00;
const TIME: u64 = 0xc01;

// ------------------------------------------------
// Machine CSR

// Machine Info registers
pub const MVENDORID: u64 = 0xf11;
pub const MARCHID: u64 = 0xf12;
pub const MIMPID: u64 = 0xf13;
pub const MHARTID: u64 = 0xf14;
const MCONFIGPTR: u64 = 0xf15;

// Machine status register
pub const MSTATUS: u64 = 0x300;
// Machine ISA
const MISA: u64 = 0x301;
// Machine interrupt pending
pub const MIP: u64 = 0x344;
// Machine interrupt enable
pub const MIE: u64 = 0x304;
// Machine trap cause
const MCAUSE: u64 = 0x342;
// Machine trap vector base address
const MTVEC: u64 = 0x305;
// Machine trap value
const MTVAL: u64 = 0x343;
// Machine exception program counter
const MEPC: u64 = 0x341;
// Machine scratch register
const MSCRATCH: u64 = 0x340;
// Machine exception delegation
const MEDELEG: u64 = 0x302;
// Machine interrupt delegation
pub const MIDELEG: u64 = 0x303;
// Machine counter enable
const MCOUNTEREN: u64 = 0x306;

// ------------------------------------------------
// Supervisor CSR

// Supervisor status register
pub const SSTATUS: u64 = 0x100;
// Supervisor interrupt pending
pub const SIP: u64 = 0x144;
// Supervisor interrupt enable
pub const SIE: u64 = 0x104;
// Supervisor trap cause
const SCAUSE: u64 = 0x142;
// Supervisor trap vector base address
const STVEC: u64 = 0x105;
// Supervisor trap value
const STVAL: u64 = 0x143;
// Supervisor exception program counter
const SEPC: u64 = 0x141;
// Supervisor scratch register
const SSCRATCH: u64 = 0x140;
// Supervisor exception delegation
const SEDELEG: u64 = 0x102;
// Supervisor interrupt delegation
const SIDELEG: u64 = 0x103;
// Supervisor counter enable
const SATP: u64 = 0x180;

pub const SSTATUS_WPRI: u64 = bitmask(34, 62) | bitmask(20, 31) | bit!(17) | bitmask(11, 12) |
    bit!(7) | bitmask(2, 4) | bit!(0);
pub const SSTATUS_MASK: u64 = !SSTATUS_WPRI;

