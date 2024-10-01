use cranelift::prelude::*;

use crate::memory::MemoryAccessSize;
use crate::riscv::trap::{RiscvTrap, RiscvTrapCause};
use super::translator::{IntegerSign, RiscvTranslator, TranslationContext};
use log::{error, debug};

pub const fn bitmask(start: u8, end: u8) -> u64 {
    assert!(start <= end);
    assert!(end < 64);

    let mut mask = 0;
    let mut i = start;
    while i <= end {
        mask |= 1 << i;
        i += 1;
    }
    mask
}

#[macro_export]
macro_rules! bitmask_field {
    ($val:expr, $start:expr, $end:expr) => {
        (($val & bitmask($start, $end)) >> $start)
    };
}

#[macro_export]
macro_rules! bitmask_field_shift {
    ($val:expr, $start:expr, $end:expr, $shift:expr) => {
        (bitmask_field!($val, $start, $end) << $shift)
    };
}

#[macro_export]
macro_rules! bit {
    ($bit:expr) => {
        (1 << $bit)
    };
}

fn sign_extend_imm(inst: &RiscvInstructionFields) -> i64 {
    match inst.format {
        InstructionFormat::I => {
            let mut imm = inst.imm as u32;
            imm = if (imm & 0x800) != 0 { imm | 0xfffff000 } else { imm };
            imm as i32 as i64
        },
        InstructionFormat::B => {
            let mut imm = inst.imm as u32;
            imm = if (imm & 0x1000) != 0 { imm | 0xffffe000 } else { imm };
            imm as i32 as i64
        },
        InstructionFormat::J => {
            let mut imm = inst.imm as u32;
            imm = if (imm & 0x00080000) != 0 { imm | 0xfff00000 } else { imm };
            imm as i32 as i64
        },
        _ => {
            error!("Unsupported instruction format");
            inst.imm as i64
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Opcode {
    // RV32I, RV64I, RV32M, RV64M
    Lui = 0x37,
    Auipc = 0x17,
    Jal = 0x6F,
    Jalr = 0x67,
    Branch = 0x63,
    Load = 0x03,
    Store = 0x23,
    ArithImm = 0x13,
    Arith = 0x33,
    Fence = 0x0F,
    System = 0x73,
    // RV64I, RV64M
    ArithImmW = 0x1B,
    ArithW = 0x3B,
    // RV32A, RV64A
    Atomic = 0x2F,
    // RV32F, RV64F
    FpLoad = 0x07,
    FpStore = 0x27,
    Fmadd = 0x43,
    Fmsub = 0x47,
    Fnmsub = 0x4B,
    Fnmadd = 0x4F,
    FpArith = 0x53,
}

pub enum InstructionIndex {
    // RV32I
    Lui,
    Auipc,
    Jal,
    Jalr,
    Beq,
    Bne,
    Blt,
    Bge,
    Bltu,
    Bgeu,
    Lb,
    Lh,
    Lw,
    Lbu,
    Lhu,
    Sb,
    Sh,
    Sw,
    Addi,
    Slti,
    Sltiu,
    Xori,
    Ori,
    Andi,
    Slli,
    Srli,
    Srai,
    Add,
    Sub,
    Sll,
    Slt,
    Sltu,
    Xor,
    Srl,
    Sra,
    Or,
    And,
    Fence,
    Ecall,
    Ebreak,
    // RV64I
    Lwu,
    Ld,
    Sd,
    Addiw,
    Slliw,
    Srliw,
    Sraiw,
    Addw,
    Subw,
    Sllw,
    Srlw,
    Sraw,
    // RV32/RV64 Zifencei
    Zifencei,
    // RV32/RV64 Zicsr
    Csrrw,
    Csrrs,
    Csrrc,
    Csrrwi,
    Csrrsi,
    Csrrci,
    // RV32M
    Mul,
    Mulh,
    Mulhsu,
    Mulhu,
    Div,
    Divu,
    Rem,
    Remu,
    // RV64M
    Mulw,
    Divw,
    Divuw,
    Remw,
    Remuw,
    // RV32A
    LrW,
    ScW,
    AmoswapW,
    AmoaddW,
    AmoxorW,
    AmoandW,
    AmoorW,
    AmominW,
    AmomaxW,
    AmominuW,
    AmomaxuW,
    // RV64A
    LrD,
    ScD,
    AmoswapD,
    AmoaddD,
    AmoxorD,
    AmoandD,
    AmoorD,
    AmominD,
    AmomaxD,
    AmominuD,
    AmomaxuD,
}

impl Opcode {
    fn get_format(&self) -> Result<InstructionFormat, RiscvTrap> {
        match self {
            Opcode::Arith | Opcode::Atomic
                | Opcode::System => Ok(InstructionFormat::R),
            Opcode::ArithImm | Opcode::ArithImmW
                | Opcode::Load | Opcode::Jalr
                | Opcode::Fence => Ok(InstructionFormat::I),
            Opcode::Store => Ok(InstructionFormat::S),
            Opcode::Branch => Ok(InstructionFormat::B),
            Opcode::Lui | Opcode::Auipc => Ok(InstructionFormat::U),
            Opcode::Jal => Ok(InstructionFormat::J),
            // TODO: Implement the rest of the formats
            _ => {
                error!("Unsupported instruction format");
                Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0))
            }
        }
    }
}

impl TryFrom<u64> for Opcode {
    type Error = RiscvTrap;
    fn try_from(value: u64) -> Result<Self, Self::Error> {
        match value {
            0x33 => Ok(Opcode::Arith),
            0x13 => Ok(Opcode::ArithImm),
            0x03 => Ok(Opcode::Load),
            0x67 => Ok(Opcode::Jalr),
            0x23 => Ok(Opcode::Store),
            0x63 => Ok(Opcode::Branch),
            0x37 => Ok(Opcode::Lui),
            0x17 => Ok(Opcode::Auipc),
            0x6F => Ok(Opcode::Jal),
            0x73 => Ok(Opcode::System),
            0x0F => Ok(Opcode::Fence),
            0x1B => Ok(Opcode::ArithImmW),
            0x3B => Ok(Opcode::ArithW),
            0x2F => Ok(Opcode::Atomic),
            _ => Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum InstructionFormat {
    R,
    I,
    S,
    B,
    U,
    J,
}

type InstructionHandler = fn(translator: &mut RiscvTranslator, ctx: &mut TranslationContext) -> Result<Option<Value>, RiscvTrap>;

#[derive(Debug, Clone, Copy)]
pub struct RiscvInstructionFields {
    format: InstructionFormat,
    raw: u64,
    opcode: u64,
    rd: u64,
    funct3: u64,
    rs1: u64,
    rs2: u64,
    funct7: u64,
    imm: u64,
}

#[derive(Debug)]
pub struct RiscvInstruction<'a> {
    pub name: &'a str,
    pub format: InstructionFormat,
    pub translate: InstructionHandler,
}

pub fn decode(inst: u64) -> Result<RiscvInstructionFields, RiscvTrap> {
    let opcode = bitmask_field!(inst, 0, 6);
    let opcode: Opcode = Opcode::try_from(opcode)?;

    let format = match opcode.get_format() {
        Ok(f) => f,
        Err(_) => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
    };

    match format {
        InstructionFormat::R => {
            Ok(RiscvInstructionFields {
                format,
                raw: inst,
                opcode: bitmask_field!(inst, 0, 6),
                rd: bitmask_field!(inst, 7, 11),
                funct3: bitmask_field!(inst, 12, 14),
                rs1: bitmask_field!(inst, 15, 19),
                rs2: bitmask_field!(inst, 20, 24),
                funct7: bitmask_field!(inst, 25, 31),
                imm: 0,
            })
        },
        InstructionFormat::I => {
            Ok(RiscvInstructionFields {
                format,
                raw: inst,
                opcode: bitmask_field!(inst, 0, 6),
                rd: bitmask_field!(inst, 7, 11),
                funct3: bitmask_field!(inst, 12, 14),
                rs1: bitmask_field!(inst, 15, 19),
                rs2: 0,
                funct7: 0,
                imm: bitmask_field!(inst, 20, 31),
            })
        },
        InstructionFormat::S => {
            Ok(RiscvInstructionFields {
                format,
                raw: inst,
                opcode: bitmask_field!(inst, 0, 6),
                rd: 0,
                funct3: bitmask_field!(inst, 12, 14),
                rs1: bitmask_field!(inst, 15, 19),
                rs2: bitmask_field!(inst, 20, 24),
                funct7: 0,
                imm: bitmask_field!(inst, 7, 11) | bitmask_field_shift!(inst, 25, 31, 5),
            })
        },
        InstructionFormat::B => {
            Ok(RiscvInstructionFields {
                format,
                raw: inst,
                opcode: bitmask_field!(inst, 0, 6),
                rd: 0,
                funct3: bitmask_field!(inst, 12, 14),
                rs1: bitmask_field!(inst, 15, 19),
                rs2: bitmask_field!(inst, 20, 24),
                funct7: 0,
                imm: bitmask_field_shift!(inst, 7, 7, 11) | bitmask_field_shift!(inst, 8, 11, 1)
                    | bitmask_field_shift!(inst, 25, 30, 5) | bitmask_field_shift!(inst, 31, 31, 12),
            })
        },
        InstructionFormat::U => {
            Ok(RiscvInstructionFields {
                format,
                raw: inst,
                opcode: bitmask_field!(inst, 0, 6),
                rd: bitmask_field!(inst, 7, 11),
                funct3: 0,
                rs1: 0,
                rs2: 0,
                funct7: 0,
                imm: bitmask_field_shift!(inst, 12, 31, 12),
            })
        },
        InstructionFormat::J => {
            Ok(RiscvInstructionFields {
                format,
                raw: inst,
                opcode: bitmask_field!(inst, 0, 6),
                rd: bitmask_field!(inst, 7, 11),
                funct3: 0,
                rs1: 0,
                rs2: 0,
                funct7: 0,
                imm: bitmask_field_shift!(inst, 21, 30, 1) | bitmask_field_shift!(inst, 20, 20, 11)
                    | bitmask_field_shift!(inst, 12, 19, 12) | bitmask_field_shift!(inst, 31, 31, 20),
            })
        },
    }
}

pub fn get_handler(inst: &RiscvInstructionFields) -> Result<&'static RiscvInstruction<'static>, RiscvTrap> {
    match Opcode::try_from(inst.opcode)? {
        Opcode::Lui => return Ok(&INST_HANDLERS[InstructionIndex::Lui as usize]),
        Opcode::Auipc => return Ok(&INST_HANDLERS[InstructionIndex::Auipc as usize]),
        Opcode::Jal => return Ok(&INST_HANDLERS[InstructionIndex::Jal as usize]),
        Opcode::Jalr => return Ok(&INST_HANDLERS[InstructionIndex::Jalr as usize]),
        Opcode::Branch => {
            match inst.funct3 {
                0x0 => return Ok(&INST_HANDLERS[InstructionIndex::Beq as usize]),
                0x1 => return Ok(&INST_HANDLERS[InstructionIndex::Bne as usize]),
                0x4 => return Ok(&INST_HANDLERS[InstructionIndex::Blt as usize]),
                0x5 => return Ok(&INST_HANDLERS[InstructionIndex::Bge as usize]),
                0x6 => return Ok(&INST_HANDLERS[InstructionIndex::Bltu as usize]),
                0x7 => return Ok(&INST_HANDLERS[InstructionIndex::Bgeu as usize]),
                _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
            }
        },
        Opcode::Load => {
            match inst.funct3 {
                0x0 => return Ok(&INST_HANDLERS[InstructionIndex::Lb as usize]),
                0x1 => return Ok(&INST_HANDLERS[InstructionIndex::Lh as usize]),
                0x2 => return Ok(&INST_HANDLERS[InstructionIndex::Lw as usize]),
                0x3 => return Ok(&INST_HANDLERS[InstructionIndex::Ld as usize]),
                0x4 => return Ok(&INST_HANDLERS[InstructionIndex::Lbu as usize]),
                0x5 => return Ok(&INST_HANDLERS[InstructionIndex::Lhu as usize]),
                0x6 => return Ok(&INST_HANDLERS[InstructionIndex::Lwu as usize]),
                _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
            }
        },
        Opcode::Store => {
            match inst.funct3 {
                0x0 => return Ok(&INST_HANDLERS[InstructionIndex::Sb as usize]),
                0x1 => return Ok(&INST_HANDLERS[InstructionIndex::Sh as usize]),
                0x2 => return Ok(&INST_HANDLERS[InstructionIndex::Sw as usize]),
                0x3 => return Ok(&INST_HANDLERS[InstructionIndex::Sd as usize]),
                _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
            }
        },
        Opcode::ArithImm => {
            match inst.funct3 {
                0x0 => return Ok(&INST_HANDLERS[InstructionIndex::Addi as usize]),
                0x1 => return Ok(&INST_HANDLERS[InstructionIndex::Slli as usize]),
                0x2 => return Ok(&INST_HANDLERS[InstructionIndex::Slti as usize]),
                0x3 => return Ok(&INST_HANDLERS[InstructionIndex::Sltiu as usize]),
                0x4 => return Ok(&INST_HANDLERS[InstructionIndex::Xori as usize]),
                0x6 => return Ok(&INST_HANDLERS[InstructionIndex::Ori as usize]),
                0x7 => return Ok(&INST_HANDLERS[InstructionIndex::Andi as usize]),
                0x5 => {
                    let funct6 = inst.funct7 >> 1;
                    match funct6 {
                        0x00 => return Ok(&INST_HANDLERS[InstructionIndex::Srli as usize]),
                        0x10 => return Ok(&INST_HANDLERS[InstructionIndex::Srai as usize]),
                        _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
                    }
                },
                _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
            }
        },
        Opcode::Arith => {
            match (inst.funct3, inst.funct7) {
                (0x0, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Add as usize]),
                (0x0, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Mul as usize]),
                (0x0, 0x20) => return Ok(&INST_HANDLERS[InstructionIndex::Sub as usize]),
                (0x1, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Sll as usize]),
                (0x1, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Mulh as usize]),
                (0x2, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Slt as usize]),
                (0x2, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Mulhsu as usize]),
                (0x3, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Sltu as usize]),
                (0x3, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Mulhu as usize]),
                (0x4, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Xor as usize]),
                (0x4, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Div as usize]),
                (0x5, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Srl as usize]),
                (0x5, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Divu as usize]),
                (0x5, 0x20) => return Ok(&INST_HANDLERS[InstructionIndex::Sra as usize]),
                (0x6, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Or as usize]),
                (0x6, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Rem as usize]),
                (0x7, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::And as usize]),
                (0x7, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Remu as usize]),
                _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
            }
        },
        Opcode::ArithW => {
            match (inst.funct3, inst.funct7) {
                (0x0, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Addw as usize]),
                (0x0, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Mulw as usize]),
                (0x0, 0x20) => return Ok(&INST_HANDLERS[InstructionIndex::Subw as usize]),
                (0x1, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Sllw as usize]),
                (0x4, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Divw as usize]),
                (0x5, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::Srlw as usize]),
                (0x5, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Divuw as usize]),
                (0x5, 0x20) => return Ok(&INST_HANDLERS[InstructionIndex::Sraw as usize]),
                (0x6, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Remuw as usize]),
                (0x7, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::Remw as usize]),
                _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
            }
        }
        Opcode::ArithImmW => {
            match inst.funct3 {
                0x0 => return Ok(&INST_HANDLERS[InstructionIndex::Addiw as usize]),
                0x1 => return Ok(&INST_HANDLERS[InstructionIndex::Slliw as usize]),
                0x5 => {
                    match inst.funct7 {
                        0x00 => return Ok(&INST_HANDLERS[InstructionIndex::Srliw as usize]),
                        0x20 => return Ok(&INST_HANDLERS[InstructionIndex::Sraiw as usize]),
                        _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
                    }
                }
                _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
            }
        },
        Opcode::Atomic => {
            let funct5 = inst.funct7 >> 2;
            let _aq = inst.funct7 & 0x2;
            let _rl = inst.funct7 & 0x1;
            match (inst.funct3, funct5) {
                (0x2, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::AmoaddW as usize]),
                (0x2, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::AmoswapW as usize]),
                (0x2, 0x02) => return Ok(&INST_HANDLERS[InstructionIndex::LrW as usize]),
                (0x2, 0x03) => return Ok(&INST_HANDLERS[InstructionIndex::ScW as usize]),
                (0x2, 0x04) => return Ok(&INST_HANDLERS[InstructionIndex::AmoxorW as usize]),
                (0x2, 0x08) => return Ok(&INST_HANDLERS[InstructionIndex::AmoorW as usize]),
                (0x2, 0x0c) => return Ok(&INST_HANDLERS[InstructionIndex::AmoandW as usize]),
                (0x2, 0x10) => return Ok(&INST_HANDLERS[InstructionIndex::AmominW as usize]),
                (0x2, 0x14) => return Ok(&INST_HANDLERS[InstructionIndex::AmomaxW as usize]),
                (0x2, 0x18) => return Ok(&INST_HANDLERS[InstructionIndex::AmominuW as usize]),
                (0x2, 0x1c) => return Ok(&INST_HANDLERS[InstructionIndex::AmomaxuW as usize]),
                (0x3, 0x00) => return Ok(&INST_HANDLERS[InstructionIndex::AmoaddD as usize]),
                (0x3, 0x01) => return Ok(&INST_HANDLERS[InstructionIndex::AmoswapD as usize]),
                (0x3, 0x02) => return Ok(&INST_HANDLERS[InstructionIndex::LrD as usize]),
                (0x3, 0x03) => return Ok(&INST_HANDLERS[InstructionIndex::ScD as usize]),
                (0x3, 0x04) => return Ok(&INST_HANDLERS[InstructionIndex::AmoxorD as usize]),
                (0x3, 0x08) => return Ok(&INST_HANDLERS[InstructionIndex::AmoorD as usize]),
                (0x3, 0x0c) => return Ok(&INST_HANDLERS[InstructionIndex::AmoandD as usize]),
                (0x3, 0x10) => return Ok(&INST_HANDLERS[InstructionIndex::AmominD as usize]),
                (0x3, 0x14) => return Ok(&INST_HANDLERS[InstructionIndex::AmomaxD as usize]),
                (0x3, 0x18) => return Ok(&INST_HANDLERS[InstructionIndex::AmominuD as usize]),
                (0x3, 0x1c) => return Ok(&INST_HANDLERS[InstructionIndex::AmomaxuD as usize]),
                _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
            }
        },
        Opcode::Fence => {
                return Ok(&INST_HANDLERS[InstructionIndex::Fence as usize]);
        }
        Opcode::System => {
            match inst.funct3 {
                0x0 => return Ok(&INST_HANDLERS[InstructionIndex::Ecall as usize]),
                0x1 => return Ok(&INST_HANDLERS[InstructionIndex::Csrrw as usize]),
                0x2 => return Ok(&INST_HANDLERS[InstructionIndex::Csrrs as usize]),
                0x3 => return Ok(&INST_HANDLERS[InstructionIndex::Csrrc as usize]),
                0x5 => return Ok(&INST_HANDLERS[InstructionIndex::Csrrwi as usize]),
                0x6 => return Ok(&INST_HANDLERS[InstructionIndex::Csrrsi as usize]),
                0x7 => return Ok(&INST_HANDLERS[InstructionIndex::Csrrci as usize]),
                _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
            }
        },
        _ => return Err(RiscvTrap::new(RiscvTrapCause::IllegalInstruction, 0)),
    }
}

static INST_HANDLERS: &[RiscvInstruction] = &[
    // RV32I
    // INSTRUCTIONS IN RV32I: LUI, AUIPC, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU,
    //                        LB, LH, LW, LBU, LHU, SB, SH, SW,
    //                        ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI, SRAI,
    //                        ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND
    RiscvInstruction {
        name: "LUI",
        format: InstructionFormat::U,
        translate: |translator, ctx| {
            let val = ctx.builder.ins().iconst(types::I64, ctx.inst.imm as i32 as i64);
            translator.reg_write(ctx, ctx.inst.rd, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AUIPC",
        format: InstructionFormat::U,
        translate: |translator, ctx| {
            let pc = ctx.builder.ins().iconst(types::I64, ctx.pc as i64);
            let val = ctx.builder.ins().iadd_imm(pc, ctx.inst.imm as i32 as i64);
            translator.reg_write(ctx, ctx.inst.rd, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "JAL",
        format: InstructionFormat::J,
        translate: |translator, ctx| {
            let mut pc_val = ctx.builder.ins().iconst(types::I64, ctx.pc as i64);
            let target = ctx.builder.ins().iadd_imm(pc_val, sign_extend_imm(&ctx.inst));
            pc_val = ctx.builder.ins().iadd_imm(pc_val, 4);
            translator.reg_write(ctx, ctx.inst.rd, pc_val);
            Ok(Some(target))
        },
    },
    RiscvInstruction {
        name: "JALR",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let mut pc_val = ctx.builder.ins().iconst(types::I64, ctx.pc as i64);
            let mut target = ctx.builder.ins().iadd_imm(val, sign_extend_imm(&ctx.inst));
            pc_val = ctx.builder.ins().iadd_imm(pc_val, 4);
            translator.reg_write(ctx, ctx.inst.rd, pc_val);
            target = ctx.builder.ins().band_imm(target, !1);
            Ok(Some(target))
        },
    },
    RiscvInstruction {
        name: "BEQ",
        format: InstructionFormat::B,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::Equal, val1, val2);
            let mut fallthrough_pc = ctx.builder.ins().iconst(types::I64, ctx.pc as i64);
            let target = ctx.builder.ins().iadd_imm(fallthrough_pc, sign_extend_imm(&ctx.inst));
            fallthrough_pc = ctx.builder.ins().iadd_imm(fallthrough_pc, 4);
            let next_pc = ctx.builder.ins().select(cmp, target, fallthrough_pc);
            Ok(Some(next_pc))
        },
    },
    RiscvInstruction {
        name: "BNE",
        format: InstructionFormat::B,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::NotEqual, val1, val2);
            let mut fallthrough_pc = ctx.builder.ins().iconst(types::I64, ctx.pc as i64);
            let target = ctx.builder.ins().iadd_imm(fallthrough_pc, sign_extend_imm(&ctx.inst));
            fallthrough_pc = ctx.builder.ins().iadd_imm(fallthrough_pc, 4);
            let next_pc = ctx.builder.ins().select(cmp, target, fallthrough_pc);
            Ok(Some(next_pc))
        },
    },
    RiscvInstruction {
        name: "BLT",
        format: InstructionFormat::B,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::SignedLessThan, val1, val2);
            let mut fallthrough_pc = ctx.builder.ins().iconst(types::I64, ctx.pc as i64);
            let target = ctx.builder.ins().iadd_imm(fallthrough_pc, sign_extend_imm(&ctx.inst));
            fallthrough_pc = ctx.builder.ins().iadd_imm(fallthrough_pc, 4);
            let next_pc = ctx.builder.ins().select(cmp, target, fallthrough_pc);
            Ok(Some(next_pc))
        },
    },
    RiscvInstruction {
        name: "BGE",
        format: InstructionFormat::B,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, val1, val2);
            let mut fallthrough_pc = ctx.builder.ins().iconst(types::I64, ctx.pc as i64);
            let target = ctx.builder.ins().iadd_imm(fallthrough_pc, sign_extend_imm(&ctx.inst));
            fallthrough_pc = ctx.builder.ins().iadd_imm(fallthrough_pc, 4);
            let next_pc = ctx.builder.ins().select(cmp, target, fallthrough_pc);
            Ok(Some(next_pc))
        },
    },
    RiscvInstruction {
        name: "BLTU",
        format: InstructionFormat::B,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, val1, val2);
            let mut fallthrough_pc = ctx.builder.ins().iconst(types::I64, ctx.pc as i64);
            let target = ctx.builder.ins().iadd_imm(fallthrough_pc, sign_extend_imm(&ctx.inst));
            fallthrough_pc = ctx.builder.ins().iadd_imm(fallthrough_pc, 4);
            let next_pc = ctx.builder.ins().select(cmp, target, fallthrough_pc);
            Ok(Some(next_pc))
        },
    },
    RiscvInstruction {
        name: "BGEU",
        format: InstructionFormat::B,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, val1, val2);
            let mut fallthrough_pc = ctx.builder.ins().iconst(types::I64, ctx.pc as i64);
            let target = ctx.builder.ins().iadd_imm(fallthrough_pc, sign_extend_imm(&ctx.inst));
            fallthrough_pc = ctx.builder.ins().iadd_imm(fallthrough_pc, 4);
            let next_pc = ctx.builder.ins().select(cmp, target, fallthrough_pc);
            Ok(Some(next_pc))
        },
    },
    RiscvInstruction {
        name: "LB",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Byte, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "LH",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.generate_memory_read(ctx, addr, MemoryAccessSize::HalfWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "LW",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "LBU",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Byte, IntegerSign::Unsigned);
            translator.reg_write(ctx, ctx.inst.rd, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "LHU",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.generate_memory_read(ctx, addr, MemoryAccessSize::HalfWord, IntegerSign::Unsigned);
            translator.reg_write(ctx, ctx.inst.rd, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SB",
        format: InstructionFormat::S,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.reg_read(ctx, ctx.inst.rs2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Byte, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SH",
        format: InstructionFormat::S,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.reg_read(ctx, ctx.inst.rs2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::HalfWord, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SW",
        format: InstructionFormat::S,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.reg_read(ctx, ctx.inst.rs2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "ADDI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let result = ctx.builder.ins().iadd_imm(val, sign_extend_imm(&ctx.inst));
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SLTI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let result = ctx.builder.ins().icmp_imm(IntCC::SignedLessThan, val, sign_extend_imm(&ctx.inst));
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SLTIU",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let result = ctx.builder.ins().icmp_imm(IntCC::UnsignedLessThan, val, ctx.inst.imm as i64);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "XORI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let result = ctx.builder.ins().bxor_imm(val, sign_extend_imm(&ctx.inst));
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "ORI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let result = ctx.builder.ins().bor_imm(val, sign_extend_imm(&ctx.inst));
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "ANDI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let result = ctx.builder.ins().band_imm(val, sign_extend_imm(&ctx.inst));
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SLLI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let shamt = ctx.inst.imm & 0x3f;
            let result = ctx.builder.ins().ishl_imm(val, shamt as i64);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SRLI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let shamt = ctx.inst.imm & 0x3f;
            let result = ctx.builder.ins().ushr_imm(val, shamt as i64);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SRAI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let shamt = ctx.inst.imm & 0x3f;
            let result = ctx.builder.ins().sshr_imm(val, shamt as i64);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "ADD",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().iadd(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SUB",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().isub(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SLL",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().ishl(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SLT",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().icmp(IntCC::SignedLessThan, val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SLTU",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "XOR",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().bxor(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SRL",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().ushr(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SRA",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().sshr(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "OR",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().bor(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "AND",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().band(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "FENCE",
        format: InstructionFormat::I,
        translate: |_translator, _ctx| {
            // TODO: Implement
            debug!("FENCE not implemented");
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "ECALL",
        format: InstructionFormat::I,
        translate: |_translator, _ctx| {
            // TODO: Implement
            debug!("ECALL not implemented");
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "EBREAK",
        format: InstructionFormat::I,
        translate: |_translator, _ctx| {
            // TODO: Implement
            debug!("EBREAK not implemented");
            Ok(None)
        }
    },
    // RV32I
    // ----------------------------------------
    // RV64I
    // INSTRUCTIONS IN RV64I: LWU, LD, SD,
    //                        ADDIW, SLLIW, SRLIW, SRAIW, ADDW, SUBW, SLLW, SRLW, SRAW
    RiscvInstruction {
        name: "LWU",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Unsigned);
            translator.reg_write(ctx, ctx.inst.rd, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "LD",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SD",
        format: InstructionFormat::S,
        translate: |translator, ctx| {
            let offset = ctx.builder.ins().iconst(types::I64, sign_extend_imm(&ctx.inst));
            let mut addr = translator.reg_read(ctx, ctx.inst.rs1);
            addr = ctx.builder.ins().iadd(addr, offset);
            let val = translator.reg_read(ctx, ctx.inst.rs2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "ADDIW",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let result = ctx.builder.ins().iadd_imm(val, sign_extend_imm(&ctx.inst) as i32 as i64);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SLLIW",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let shamt = ctx.inst.imm & 0x1f;
            let result = ctx.builder.ins().ishl_imm(val, shamt as i64);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SRLIW",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let shamt = ctx.inst.imm & 0x1f;
            let result = ctx.builder.ins().ushr_imm(val, shamt as i64);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SRAIW",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val = translator.reg_read(ctx, ctx.inst.rs1);
            let shamt = ctx.inst.imm & 0x1f;
            let result = ctx.builder.ins().sshr_imm(val, shamt as i64);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "ADDW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let mut result = ctx.builder.ins().iadd(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SUBW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let mut result = ctx.builder.ins().isub(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SLLW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let mut result = ctx.builder.ins().ishl(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SRLW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let mut result = ctx.builder.ins().ushr(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "SRAW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let mut result = ctx.builder.ins().sshr(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        },
    },
    // RV64I
    // ----------------------------------------
    // RV32/RV64 Zifencei
    // INSTRUCTIONS IN RV32/RV64 Zifencei: FENCE.I
    RiscvInstruction {
        name: "FENCE.I",
        format: InstructionFormat::I,
        translate: |_translator, _ctx| {
            // TODO: Implement
            debug!("FENCE.I not implemented");
            Ok(None)
        }
    },
    // RV32/RV64 Zifencei
    // ----------------------------------------
    // RV32/RV64 Zicsr
    // INSTRUCTIONS IN RV32/RV64 Zicsr: CSRRW, CSRRS, CSRRC, CSRRWI, CSRRSI, CSRRCI
    RiscvInstruction {
        name: "CSRRW",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val1 = translator.csr_read(ctx, ctx.inst.imm);
            let val2 = translator.reg_read(ctx, ctx.inst.rs1);
            translator.csr_write(ctx, ctx.inst.imm, val2);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "CSRRS",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val1 = translator.csr_read(ctx, ctx.inst.imm);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs1);
            val2 = ctx.builder.ins().bor(val1, val2);
            translator.csr_write(ctx, ctx.inst.imm, val2);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "CSRRC",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val1 = translator.csr_read(ctx, ctx.inst.imm);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs1);
            val2 = ctx.builder.ins().band_not(val1, val2);
            translator.csr_write(ctx, ctx.inst.imm, val2);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "CSRRWI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val1 = translator.csr_read(ctx, ctx.inst.imm);
            let val2 = ctx.builder.ins().iconst(types::I64, ctx.inst.rs1 as i64);
            translator.csr_write(ctx, ctx.inst.imm, val2);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "CSRRSI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val1 = translator.csr_read(ctx, ctx.inst.imm);
            let mut val2 = ctx.builder.ins().iconst(types::I64, ctx.inst.rs1 as i64);
            val2 = ctx.builder.ins().bor(val1, val2);
            translator.csr_write(ctx, ctx.inst.imm, val2);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "CSRRCI",
        format: InstructionFormat::I,
        translate: |translator, ctx| {
            let val1 = translator.csr_read(ctx, ctx.inst.imm);
            let mut val2 = ctx.builder.ins().iconst(types::I64, ctx.inst.rs1 as i64);
            val2 = ctx.builder.ins().band_not(val1, val2);
            translator.csr_write(ctx, ctx.inst.imm, val2);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            Ok(None)
        }
    },
    // RV32/RV64 Zicsr
    // ----------------------------------------
    // RV32M
    // INSTRUCTIONS IN RV32M: MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU
    RiscvInstruction {
        name: "MUL",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().imul(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "MULH",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let mut val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs2);
            val1 = ctx.builder.ins().sextend(types::I128, val1);
            val2 = ctx.builder.ins().sextend(types::I128, val2);
            let mut result = ctx.builder.ins().imul(val1, val2);
            result = ctx.builder.ins().ushr_imm(result, 64);
            result = ctx.builder.ins().ireduce(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "MULHSU",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let mut val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs2);
            val1 = ctx.builder.ins().sextend(types::I128, val1);
            val2 = ctx.builder.ins().uextend(types::I128, val2);
            let mut result = ctx.builder.ins().imul(val1, val2);
            result = ctx.builder.ins().ushr_imm(result, 64);
            result = ctx.builder.ins().ireduce(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "MULHU",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let mut val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs2);
            val1 = ctx.builder.ins().uextend(types::I128, val1);
            val2 = ctx.builder.ins().uextend(types::I128, val2);
            let mut result = ctx.builder.ins().imul(val1, val2);
            result = ctx.builder.ins().ushr_imm(result, 64);
            result = ctx.builder.ins().ireduce(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "DIV",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            // TODO: Handle division by zero and overflow
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().sdiv(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "DIVU",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            // TODO: Handle division by zero and overflow
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().udiv(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "REM",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            // TODO: Handle division by zero and overflow
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().srem(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "REMU",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            // TODO: Handle division by zero and overflow
            let val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let result = ctx.builder.ins().urem(val1, val2);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    // RV32M
    // ----------------------------------------
    // RV64M
    // INSTRUCTIONS IN RV64M: MULW, DIVW, DIVUW, REMW, REMUW
    RiscvInstruction {
        name: "MULW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let mut val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs2);
            val1 = ctx.builder.ins().ireduce(types::I32, val1);
            val2 = ctx.builder.ins().ireduce(types::I32, val2);
            let mut result = ctx.builder.ins().imul(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "DIVW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            // TODO: Handle division by zero and overflow
            let mut val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs2);
            val1 = ctx.builder.ins().ireduce(types::I32, val1);
            val2 = ctx.builder.ins().ireduce(types::I32, val2);
            let mut result = ctx.builder.ins().sdiv(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "DIVUW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            // TODO: Handle division by zero and overflow
            let mut val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs2);
            val1 = ctx.builder.ins().ireduce(types::I32, val1);
            val2 = ctx.builder.ins().ireduce(types::I32, val2);
            let mut result = ctx.builder.ins().udiv(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "REMW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            // TODO: Handle division by zero and overflow
            let mut val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs2);
            val1 = ctx.builder.ins().ireduce(types::I32, val1);
            val2 = ctx.builder.ins().ireduce(types::I32, val2);
            let mut result = ctx.builder.ins().srem(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "REMUW",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            // TODO: Handle division by zero and overflow
            let mut val1 = translator.reg_read(ctx, ctx.inst.rs1);
            let mut val2 = translator.reg_read(ctx, ctx.inst.rs2);
            val1 = ctx.builder.ins().ireduce(types::I32, val1);
            val2 = ctx.builder.ins().ireduce(types::I32, val2);
            let mut result = ctx.builder.ins().urem(val1, val2);
            result = ctx.builder.ins().sextend(types::I64, result);
            translator.reg_write(ctx, ctx.inst.rd, result);
            Ok(None)
        }
    },
    // RV64M
    // ----------------------------------------
    // RV32A
    // INSTRUCTIONS IN RV32A: LR.W, SC.W, AMOSWAP.W, AMOADD.W, AMOXOR.W, AMOAND.W, AMOOR.W, AMOMIN.W, AMOMAX.W, AMOMINU.W, AMOMAXU.W
    RiscvInstruction {
        name: "LR.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, addr);
            // TODO: Implement reservation set
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SC.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            let val = translator.reg_read(ctx, ctx.inst.rs2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, val);
            // TODO: Implement reservation set
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOSWAP.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let mut val = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val);
            // Write rs2 to addr, performing a swap
            val = translator.reg_read(ctx, ctx.inst.rs2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOADD.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Add rs2 to the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let res = ctx.builder.ins().iadd(val1, val2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOXOR.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // XOR rs2 with the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let res = ctx.builder.ins().bxor(val1, val2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOAND.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // AND rs2 with the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let res = ctx.builder.ins().band(val1, val2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOOR.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // OR rs2 with the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let res = ctx.builder.ins().bor(val1, val2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOMIN.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Compute the minimum of rs2 and the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::SignedLessThan, val2, val1);
            let res = ctx.builder.ins().select(
                cmp,
                val2,
                val1,
            );
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOMAX.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Compute the maximum of rs2 and the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::SignedGreaterThan, val2, val1);
            let res = ctx.builder.ins().select(
                cmp,
                val2,
                val1,
            );
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOMINU.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Compute the minimum of rs2 and the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, val2, val1);
            let res = ctx.builder.ins().select(
                cmp,
                val2,
                val1,
            );
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOMAXU.W",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::Word, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Compute the maximum of rs2 and the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::UnsignedGreaterThan, val2, val1);
            let res = ctx.builder.ins().select(
                cmp,
                val2,
                val1,
            );
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::Word, res);
            Ok(None)
        },
    },
    // RV32A
    // ----------------------------------------
    // RV64A
    // INSTRUCTIONS IN RV64A: LR.D, SC.D, AMOSWAP.D, AMOADD.D, AMOXOR.D, AMOAND.D, AMOOR.D, AMOMIN.D, AMOMAX.D, AMOMINU.D, AMOMAXU.D
    RiscvInstruction {
        name: "LR.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, addr);
            // TODO: Implement reservation set
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "SC.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            let val = translator.reg_read(ctx, ctx.inst.rs2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, val);
            // TODO: Implement reservation set
            Ok(None)
        }
    },
    RiscvInstruction {
        name: "AMOSWAP.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let mut val = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val);
            // Write rs2 to addr, performing a swap
            val = translator.reg_read(ctx, ctx.inst.rs2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, val);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOADD.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Add rs2 to the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let res = ctx.builder.ins().iadd(val1, val2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOXOR.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // XOR rs2 with the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let res = ctx.builder.ins().bxor(val1, val2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOAND.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // AND rs2 with the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let res = ctx.builder.ins().band(val1, val2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOOR.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // OR rs2 with the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let res = ctx.builder.ins().bor(val1, val2);
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOMIN.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Compute the minimum of rs2 and the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::SignedLessThan, val2, val1);
            let res = ctx.builder.ins().select(
                cmp,
                val2,
                val1,
            );
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOMAX.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Compute the maximum of rs2 and the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::SignedGreaterThan, val2, val1);
            let res = ctx.builder.ins().select(
                cmp,
                val2,
                val1,
            );
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOMINU.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Compute the minimum of rs2 and the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::UnsignedLessThan, val2, val1);
            let res = ctx.builder.ins().select(
                cmp,
                val2,
                val1,
            );
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, res);
            Ok(None)
        },
    },
    RiscvInstruction {
        name: "AMOMAXU.D",
        format: InstructionFormat::R,
        translate: |translator, ctx| {
            let addr = translator.reg_read(ctx, ctx.inst.rs1);
            // Load from addr in rs1 and write to rd
            let val1 = translator.generate_memory_read(ctx, addr, MemoryAccessSize::DoubleWord, IntegerSign::Signed);
            translator.reg_write(ctx, ctx.inst.rd, val1);
            // Compute the maximum of rs2 and the value at addr
            let val2 = translator.reg_read(ctx, ctx.inst.rs2);
            let cmp = ctx.builder.ins().icmp(IntCC::UnsignedGreaterThan, val2, val1);
            let res = ctx.builder.ins().select(
                cmp,
                val2,
                val1,
            );
            translator.generate_memory_write(ctx, addr, MemoryAccessSize::DoubleWord, res);
            Ok(None)
        },
    },
    // RV64A
    // ----------------------------------------
];

