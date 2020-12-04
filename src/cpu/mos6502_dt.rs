use super::mos6502::{mos6502_addressing_modes::AddrMode, *};
use crate::instr;
use lazy_static::lazy_static;
use std::collections::HashMap;

lazy_static! {
pub static ref DECODING_TABLE: HashMap<u8, Instruction> = hashmap! {
    0x69 => instr("ADC", 0x69, Cpu::adc, AddrMode::Immediate, 2, 2),

    0x65 => instr("ADC", 0x65, Cpu::adc, AddrMode::ZeroPage, 3, 2),

    0x75 => instr("ADC", 0x75, Cpu::adc, AddrMode::ZeroPageX, 4, 2),

    0x6d => instr("ADC", 0x6d, Cpu::adc, AddrMode::Absolute, 4, 3),

    0x7d => instr("ADC", 0x7d, Cpu::adc, AddrMode::IndexedX, 4, 3),

    0x79 => instr("ADC", 0x79, Cpu::adc, AddrMode::IndexedY, 4, 3),

    0x61 => instr("ADC", 0x61, Cpu::adc, AddrMode::IndexedIndirect, 6, 2),

    0x71 => instr("ADC", 0x71, Cpu::adc, AddrMode::IndirectIndexed, 5, 2),

    0x29 => instr("AND", 0x29, Cpu::and, AddrMode::Immediate, 2, 2),

    0x25 => instr("AND", 0x25, Cpu::and, AddrMode::ZeroPage, 3, 2),

    0x35 => instr("AND", 0x35, Cpu::and, AddrMode::ZeroPageX, 4, 2),

    0x2d => instr("AND", 0x2d, Cpu::and, AddrMode::Absolute, 4, 3),

    0x3d => instr("AND", 0x3d, Cpu::and, AddrMode::IndexedX, 4, 3),

    0x39 => instr("AND", 0x39, Cpu::and, AddrMode::IndexedY, 4, 3),

    0x21 => instr("AND", 0x21, Cpu::and, AddrMode::IndexedIndirect, 6, 2),

    0x31 => instr("AND", 0x31, Cpu::and, AddrMode::IndirectIndexed, 5, 2),

    0x0a => instr("ASL", 0x0a, Cpu::asl, AddrMode::Accumulator, 2, 1),

    0x06 => instr("ASL", 0x06, Cpu::asl, AddrMode::ZeroPage, 5, 2),

    0x16 => instr("ASL", 0x16, Cpu::asl, AddrMode::ZeroPageX, 6, 2),

    0x0e => instr("ASL", 0x0e, Cpu::asl, AddrMode::Absolute, 6, 3),

    0x1e => instr("ASL", 0x1e, Cpu::asl, AddrMode::IndexedX, 7, 3),

    // Can also take 3 number of cycles.
    0x90 => instr("BCC", 0x90, Cpu::bcc, AddrMode::Relative, 2, 2),

    // Can also take 3 number of cycles.
    0xB0 => instr("BCS", 0xB0, Cpu::bcs, AddrMode::Relative, 2, 2),

    // Can also take 3 number of cycles.
    0xF0 => instr("BEQ", 0xF0, Cpu::beq, AddrMode::Relative, 2, 2),

    // Can also take 3 number of cycles.
    0x30 => instr("BMI", 0x30, Cpu::bmi, AddrMode::Relative, 2, 2),

    // Can also take 3 number of cycles.
    0xD0 => instr("BNE", 0xD0, Cpu::bne, AddrMode::Relative, 2, 2),

    // Can also take 3 number of cycles.
    0x10 => instr("BPL", 0x10, Cpu::bpl, AddrMode::Relative, 2, 2),

    // Can also take 3 number of cycles.
    0x50 => instr("BVC", 0x50, Cpu::bvc, AddrMode::Relative, 2, 2),

    // Can also take 3 number of cycles.
    0x70 => instr("BVS", 0x70, Cpu::bvs, AddrMode::Relative, 2, 2),

    0x24 => instr("BIT", 0x24, Cpu::bit, AddrMode::ZeroPage, 3, 2),

    0x2c => instr("BIT", 0x2c, Cpu::bit, AddrMode::Absolute, 4, 3),

    0x00 => instr("BRK", 0x00, Cpu::brk, AddrMode::Implied, 7, 1),

    0x18 => instr("CLC", 0x18, Cpu::clc, AddrMode::Implied, 2, 1),

    0xd8 => instr("CLD", 0xd8, Cpu::cld, AddrMode::Implied, 2, 1),

    0x58 => instr("CLI", 0x58, Cpu::cli, AddrMode::Implied, 2, 1),

    0xb8 => instr("CLV", 0xb8, Cpu::clv, AddrMode::Implied, 2, 1),

    0xea => instr("NOP", 0xea, Cpu::nop, AddrMode::Implied, 2, 1),

    0x48 => instr("PHA", 0x48, Cpu::pha, AddrMode::Implied, 3, 1),

    0x68 => instr("PLA", 0x68, Cpu::pla, AddrMode::Implied, 4, 1),

    0x08 => instr("PHP", 0x08, Cpu::php, AddrMode::Implied, 3, 1),

    0x28 => instr("PLP", 0x28, Cpu::plp, AddrMode::Implied, 4, 1),

    0x40 => instr("RTI", 0x40, Cpu::rti, AddrMode::Implied, 6, 1),

    0x60 => instr("RTS", 0x60, Cpu::rts, AddrMode::Implied, 6, 1),

    0x38 => instr("SEC", 0x38, Cpu::sec, AddrMode::Implied, 2, 1),

    0xf8 => instr("SED", 0xf8, Cpu::sed, AddrMode::Implied, 2, 1),

    0x78 => instr("SEI", 0x78, Cpu::sei, AddrMode::Implied, 2, 1),

    0xaa => instr("TAX", 0xaa, Cpu::tax, AddrMode::Implied, 2, 1),

    0x8a => instr("TXA", 0x8a, Cpu::txa, AddrMode::Implied, 2, 1),

    0xa8 => instr("TAY", 0xa8, Cpu::tay, AddrMode::Implied, 2, 1),

    0x98 => instr("TYA", 0x98, Cpu::tya, AddrMode::Implied, 2, 1),

    0xba => instr("TSX", 0xba, Cpu::tsx, AddrMode::Implied, 2, 1),

    0x9a => instr("TXS", 0x9a, Cpu::txs, AddrMode::Implied, 2, 1),

    0xc9 => instr("CMP", 0xc9, Cpu::cmp, AddrMode::Immediate, 2, 2),

    0xc5 => instr("CMP", 0xc5, Cpu::cmp, AddrMode::ZeroPage, 3, 2),

    0xd5 => instr("CMP", 0xd5, Cpu::cmp, AddrMode::ZeroPageX, 4, 2),

    0xcd => instr("CMP", 0xcd, Cpu::cmp, AddrMode::Absolute, 4, 3),

    0xdd => instr("CMP", 0xdd, Cpu::cmp, AddrMode::IndexedX, 4, 3),

    0xd9 => instr("CMP", 0xd9, Cpu::cmp, AddrMode::IndexedY, 4, 3),

    0xc1 => instr("CMP", 0xc1, Cpu::cmp, AddrMode::IndexedIndirect, 6, 2),

    0xd1 => instr("CMP", 0xd1, Cpu::cmp, AddrMode::IndirectIndexed, 5, 2),

    0xe0 => instr("CPX", 0xe0, Cpu::cpx, AddrMode::Immediate, 2, 2),

    0xe4 => instr("CPX", 0xe4, Cpu::cpx, AddrMode::ZeroPage, 3, 2),

    0xec => instr("CPX", 0xec, Cpu::cpx, AddrMode::Absolute, 4, 3),

    0xc0 => instr("CPY", 0xc0, Cpu::cpy, AddrMode::Immediate, 2, 2),

    0xc4 => instr("CPY", 0xc4, Cpu::cpy, AddrMode::ZeroPage, 3, 2),

    0xcc => instr("CPY", 0xcc, Cpu::cpy, AddrMode::Absolute, 4, 3),

    0xc6 => instr("DEC", 0xc6, Cpu::dec, AddrMode::ZeroPage, 5, 2),

    0xd6 => instr("DEC", 0xd6, Cpu::dec, AddrMode::ZeroPageX, 6, 2),

    0xce => instr("DEC", 0xce, Cpu::dec, AddrMode::Absolute, 6, 3),

    0xde => instr("DEC", 0xde, Cpu::dec, AddrMode::IndexedX, 7, 3),

    0xca => instr("DEX", 0xca, Cpu::dex, AddrMode::Implied, 2, 1),

    0x88 => instr("DEY", 0x88, Cpu::dey, AddrMode::Implied, 2, 1),

    0xe8 => instr("INX", 0xe8, Cpu::inx, AddrMode::Implied, 2, 1),

    0xc8 => instr("INY", 0xc8, Cpu::iny, AddrMode::Implied, 2, 1),

    0x49 => instr("EOR", 0x49, Cpu::eor, AddrMode::Immediate, 2, 2),

    0x45 => instr("EOR", 0x45, Cpu::eor, AddrMode::ZeroPage, 3, 2),

    0x55 => instr("EOR", 0x55, Cpu::eor, AddrMode::ZeroPageX, 4, 2),

    0x4d => instr("EOR", 0x4d, Cpu::eor, AddrMode::Absolute, 4, 3),

    0x5d => instr("EOR", 0x5d, Cpu::eor, AddrMode::IndexedX, 4, 3),

    0x59 => instr("EOR", 0x59, Cpu::eor, AddrMode::IndexedY, 4, 3),

    0x41 => instr("EOR", 0x41, Cpu::eor, AddrMode::IndexedIndirect, 6, 2),

    0x51 => instr("EOR", 0x51, Cpu::eor, AddrMode::IndirectIndexed, 5, 2),

    0xe6 => instr("INC", 0xe6, Cpu::inc, AddrMode::ZeroPage, 5, 2),

    0xf6 => instr("INC", 0xf6, Cpu::inc, AddrMode::ZeroPageX, 6, 2),

    0xee => instr("INC", 0xee, Cpu::inc, AddrMode::Absolute, 6, 3),

    0xfe => instr("INC", 0xfe, Cpu::inc, AddrMode::IndexedX, 7, 3),

    0x4c => instr("JMP", 0x4c, Cpu::jmp, AddrMode::Absolute, 3, 3),

    0x6c => instr("JMP", 0x6c, Cpu::jmp, AddrMode::Indirect, 5, 3),

    0x20 => instr("JSR", 0x20, Cpu::jsr, AddrMode::Absolute, 6, 3),

    0xa9 => instr("LDA", 0xa9, Cpu::lda, AddrMode::Immediate, 2, 2),

    0xa5 => instr("LDA", 0xa5, Cpu::lda, AddrMode::ZeroPage, 3, 2),

    0xb5 => instr("LDA", 0xb5, Cpu::lda, AddrMode::ZeroPageX, 4, 2),

    0xad => instr("LDA", 0xad, Cpu::lda, AddrMode::Absolute, 4, 3),

    0xbd => instr("LDA", 0xbd, Cpu::lda, AddrMode::IndexedX, 4, 3),

    0xb9 => instr("LDA", 0xb9, Cpu::lda, AddrMode::IndexedY, 4, 3),

    0xa1 => instr("LDA", 0xa1, Cpu::lda, AddrMode::IndexedIndirect, 6, 2),

    0xb1 => instr("LDA", 0xb1, Cpu::lda, AddrMode::IndirectIndexed, 5, 2),

    0xa2 => instr("LDX", 0xa2, Cpu::ldx, AddrMode::Immediate, 2, 2),

    0xa6 => instr("LDX", 0xa6, Cpu::ldx, AddrMode::ZeroPage, 3, 2),

    0xb6 => instr("LDX", 0xb6, Cpu::ldx, AddrMode::ZeroPageY, 4, 2),

    0xae => instr("LDX", 0xae, Cpu::ldx, AddrMode::Absolute, 4, 3),

    0xbe => instr("LDX", 0xbe, Cpu::ldx, AddrMode::IndexedY, 4, 3),

    0xa0 => instr("LDY", 0xa0, Cpu::ldy, AddrMode::Immediate, 2, 2),

    0xa4 => instr("LDY", 0xa4, Cpu::ldy, AddrMode::ZeroPage, 3, 2),

    0xb4 => instr("LDY", 0xb4, Cpu::ldy, AddrMode::ZeroPageX, 4, 2),

    0xac => instr("LDY", 0xac, Cpu::ldy, AddrMode::Absolute, 4, 3),

    0xbc => instr("LDY", 0xbc, Cpu::ldy, AddrMode::IndexedX, 4, 3),

    0x4a => instr("LSR", 0x4a, Cpu::lsr, AddrMode::Accumulator, 2, 1),

    0x46 => instr("LSR", 0x46, Cpu::lsr, AddrMode::ZeroPage, 5, 2),

    0x56 => instr("LSR", 0x56, Cpu::lsr, AddrMode::ZeroPageX, 6, 2),

    0x4e => instr("LSR", 0x4e, Cpu::lsr, AddrMode::Absolute, 6, 3),

    0x5e => instr("LSR", 0x5e, Cpu::lsr, AddrMode::IndexedX, 7, 3),

    0x09 => instr("ORA", 0x09, Cpu::ora, AddrMode::Immediate, 2, 2),

    0x05 => instr("ORA", 0x05, Cpu::ora, AddrMode::ZeroPage, 3, 2),

    0x15 => instr("ORA", 0x15, Cpu::ora, AddrMode::ZeroPageX, 4, 2),

    0x0d => instr("ORA", 0x0d, Cpu::ora, AddrMode::Absolute, 4, 3),

    0x1d => instr("ORA", 0x1d, Cpu::ora, AddrMode::IndexedX, 4, 3),

    0x19 => instr("ORA", 0x19, Cpu::ora, AddrMode::IndexedY, 4, 3),

    0x01 => instr("ORA", 0x01, Cpu::ora, AddrMode::IndexedIndirect, 6, 2),

    0x11 => instr("ORA", 0x11, Cpu::ora, AddrMode::IndirectIndexed, 5, 2),

    0x2a => instr("ROL", 0x2a, Cpu::rol, AddrMode::Accumulator, 2, 1),

    0x26 => instr("ROL", 0x26, Cpu::rol, AddrMode::ZeroPage, 5, 2),

    0x36 => instr("ROL", 0x36, Cpu::rol, AddrMode::ZeroPageX, 6, 2),

    0x2e => instr("ROL", 0x2e, Cpu::rol, AddrMode::Absolute, 6, 3),

    0x3e => instr("ROL", 0x3e, Cpu::rol, AddrMode::IndexedX, 7, 3),

    0x6a => instr("ROR", 0x6a, Cpu::ror, AddrMode::Accumulator, 2, 1),

    0x66 => instr("ROR", 0x66, Cpu::ror, AddrMode::ZeroPage, 5, 2),

    0x76 => instr("ROR", 0x76, Cpu::ror, AddrMode::ZeroPageX, 6, 2),

    0x7e => instr("ROR", 0x7e, Cpu::ror, AddrMode::Absolute, 6, 3),

    0x6e => instr("ROR", 0x6e, Cpu::ror, AddrMode::IndexedX, 7, 3),

    0xe9 => instr("SBC", 0xe9, Cpu::sbc, AddrMode::Immediate, 2, 2),

    0xe5 => instr("SBC", 0xe5, Cpu::sbc, AddrMode::ZeroPage, 3, 2),

    0xf5 => instr("SBC", 0xf5, Cpu::sbc, AddrMode::ZeroPageX, 4, 2),

    0xed => instr("SBC", 0xed, Cpu::sbc, AddrMode::Absolute, 4, 3),

    0xfd => instr("SBC", 0xfd, Cpu::sbc, AddrMode::IndexedX, 4, 3),

    0xf9 => instr("SBC", 0xf9, Cpu::sbc, AddrMode::IndexedY, 4, 3),

    0xe1 => instr("SBC", 0xe1, Cpu::sbc, AddrMode::IndexedIndirect, 6, 2),

    0xf1 => instr("SBC", 0xf1, Cpu::sbc, AddrMode::IndirectIndexed, 5, 2),

    0x85 => instr("STA", 0x85, Cpu::sta, AddrMode::ZeroPage, 3, 2),

    0x95 => instr("STA", 0x95, Cpu::sta, AddrMode::ZeroPageX, 4, 2),

    0x8d => instr("STA", 0x8d, Cpu::sta, AddrMode::Absolute, 4, 3),

    0x9d => instr("STA", 0x9d, Cpu::sta, AddrMode::IndexedX, 5, 3),

    0x99 => instr("STA", 0x99, Cpu::sta, AddrMode::IndexedY, 5, 3),

    0x81 => instr("STA", 0x81, Cpu::sta, AddrMode::IndexedIndirect, 6, 2),

    0x91 => instr("STA", 0x91, Cpu::sta, AddrMode::IndirectIndexed, 6, 2),

    0x86 => instr("STX", 0x86, Cpu::stx, AddrMode::ZeroPage, 3, 2),

    0x96 => instr("STX", 0x96, Cpu::stx, AddrMode::ZeroPageY, 4, 2),

    0x8e => instr("STX", 0x8e, Cpu::stx, AddrMode::Absolute, 4, 3),

    0x84 => instr("STY", 0x84, Cpu::sty, AddrMode::ZeroPage, 3, 2),

    0x94 => instr("STY", 0x94, Cpu::sty, AddrMode::ZeroPageX, 4, 2),

    0x8c => instr("STY", 0x8c, Cpu::sty, AddrMode::Absolute, 4, 3),


};
}
