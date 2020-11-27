#![cfg_attr(debug_assertions, allow(dead_code, unused_imports, unused_variables))]
use crate::bus::dummy_bus::*;

use self::mos6502_addressing_modes::AddrMode;
use super::mos6502_iset::*;

use lazy_static::lazy_static;
use log::*;
use std::collections::HashMap;

type InstructionPtr = fn(&mut Cpu) -> ();

pub struct Instruction {
    mnemonic: &'static str,
    opcode: u16,
    am: AddrMode,
    cycles: u8,
    bytes: u8,
    f: InstructionPtr,
}

impl Copy for Instruction {}

impl Clone for Instruction {
    fn clone(&self) -> Self {
        *self
    }
}

impl std::fmt::Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Instruction")
            .field("mnemonic", &self.mnemonic)
            .field("opcode", &self.opcode)
            .field("cycles", &self.cycles)
            .field("bytes", &self.bytes)
            .finish()
    }
}

impl Instruction {
    fn new(
        mnemonic: &'static str,
        opcode: u16,
        am: AddrMode,
        cycles: u8,
        bytes: u8,
        f: InstructionPtr,
    ) -> Self {
        Instruction {
            mnemonic,
            opcode,
            am,
            cycles,
            bytes,
            f,
        }
    }
}

#[derive(Debug)]
pub struct RegSet {
    a: u8,
    x: u8,
    y: u8,
    sp: u16,
    pc: u16,
    ps: u8,
}
impl RegSet {
    pub fn new() -> Self {
        RegSet {
            a: 0,
            x: 0,
            y: 0,
            sp: 0,
            pc: 0,
            ps: 0,
        }
    }
}

#[derive(Debug)]
pub struct Cpu {
    pub busline: Option<DummyMainBus>,
    addr_abs: u16,
    addr_rel: u16,
    fetched: u8,
    pub time: u64,
    pub current: Option<Instruction>,
    pub state: RegSet,
    additional_cycle: bool,
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            busline: Some(DummyMainBus::new()),
            addr_abs: 0,
            addr_rel: 0,
            fetched: 0,
            time: 0,
            additional_cycle: false,
            current: None,
            state: RegSet::new(),
        }
    }

    pub fn read_pc_byte(&mut self) -> u8 {
        let temp = self.state.pc;
        self.state.pc += 1;
        self.read_bus(&temp)
    }

    pub fn read_pc_word_le(&mut self) -> u16 {
        let lo = self.read_pc_byte() as u16;
        let hi = self.read_pc_byte() as u16;
        (hi << 8) | lo
    }

    pub fn read_bus(&self, addr: &u16) -> u8 {
        let mut val: u8 = 0;
        if let Some(bus) = &self.busline {
            val = bus.read(addr);
        }
        val
    }

    pub fn writ_word(&mut self, addr: &u16, val: &u16) -> bool {
        let mut a = *addr;
        let lo = (val & 0x00ff) as u8;
        let hi = (val & 0xff00) as u8;
        if self.writ_byte(&a, &lo) == false {
            return false;
        }
        a += 1;
        if self.writ_byte(&a, &hi) == false {
            return false;
        }

        true
    }

    pub fn writ_byte(&mut self, addr: &u16, val: &u8) -> bool {
        let bus = self.busline;

        if bus.is_none() {
            return false;
        }

        let mut bus = bus.unwrap();
        bus.write(addr, val);

        return true;
    }

    fn decode(&mut self) {
        // Kind of ugly.
        // todo: Clean in up, please.
        let opcode = &self.addr_abs;
        if let Some(i) = DECODING_TABLE.get(opcode) {
            self.current = Some(i.clone());
        } else {
            self.current = None;
        }
    }

    fn fetch(&mut self) {
        info!("Fetching...");
        let prev = self.state.pc.clone();
        info!("PC was {:?}", prev);
        let opcode = self.read_bus(&prev);
        info!("Next instruction is {:?}", opcode);
        self.state.pc += 1;
        info!("PC is {:?} now", opcode);
        self.addr_abs = opcode as u16;
    }

    fn execute(&mut self) {
        // remove unwrap() (it panics - add default value)
        let instr = self.current.as_ref().unwrap().clone();
        self.locate(instr.am);
        let f = instr.f;
        f(self);
    }

    pub fn tick(&mut self) {
        info!("Ticking...");
        self.fetch();
        self.decode();
        self.execute();
    }

    pub fn reset(&mut self) {
        self.state.a = 0;
        self.state.x = 0;
        self.state.y = 0;

        self.state.ps = 0;
        self.state.sp = 0;

        self.fetched = 0;

        self.addr_abs = 0;
        self.addr_rel = 0;

        self.state.pc = ResetVectors::RESET as u16;
        self.state.pc = self.read_pc_word_le();

        info!("Cpu reset...");
        info!("\tA = ${:#02x}", self.state.a);
        info!("\tX = ${:#02x}", self.state.x);
        info!("\tY = ${:#02x}", self.state.y);
        info!("\tPC = ${:#04x}", self.state.pc);
        info!("\tSP = ${:#04x}", self.state.sp);
        info!("\tP = ${:#02x}", self.state.sp);
        info!("Cpu additional...");
        info!("\tAA = ${:#04x}", self.addr_abs);
        info!("\tAR = ${:#04x}", self.addr_rel);
        info!("\tF = ${:#02x}", self.fetched);
    }

    fn locate(&mut self, mode: self::mos6502_addressing_modes::AddrMode) {
        use self::mos6502_addressing_modes::{AddrMode::*, *};

        match mode {
            Accumulator => self.acc(),
            Implied => self.imp(),
            Immediate => self.imm(),
            Relative => self.rel(),
            Absolute => self.abs(),
            ZeroPage => self.zp0(),
            ZeroPageX => self.zpx(),
            ZeroPageY => self.zpy(),
            IndexedX => self.abx(),
            IndexedY => self.aby(),
            IndexedIndirect => self.exir(),
            Indirect => self.ind(),
            IndirectIndexed => self.irex(),
        };
    }
}

// All these rely on having the next byte unused
// i.e NMI's vector is actual $fffa-$ffff, reset's - $fffc-$fffd and irq's - $fffe-$ffff
pub enum ResetVectors {
    NMI = 0xfffa,
    RESET = 0xfffc,
    IRQ = 0xfffe,
}

const FLAGS_DEFAULT: u8 = I | U;
const N: u8 = 1 << 7;
const V: u8 = 1 << 6;
const U: u8 = 1 << 5;
const B: u8 = 1 << 4;
const D: u8 = 1 << 3;
const I: u8 = 1 << 2;
const Z: u8 = 1 << 1;
const C: u8 = 1 << 0;

pub fn instr(
    mnemonic: &'static str,
    opcode: u16,
    func: InstructionPtr,
    addr: AddrMode,
    cycles: u8,
    bytes: u8,
) -> Instruction {
    Instruction::new(mnemonic, opcode, addr, cycles, bytes, func)
}

lazy_static! {
    static ref DECODING_TABLE: HashMap<u16, Instruction> = hashmap! {
         0x00 => instr("TEST", 0x00, mos6502_instruction_set::nop, AddrMode::Accumulator, 0, 0),
    };
}

mod mos6502_instruction_set {
    use super::*;

    pub fn nop(cpu: &mut Cpu) {}
}

impl Cpu {
    #[must_use]
    pub fn same_page(p: u16, q: u16) -> bool {
        (p & 0xFF00) == (q & 0xFF00)
    }
}

impl Cpu {
    pub fn acc(&mut self) {
        self.fetched = self.state.a;
        self.additional_cycle = false;
    }

    pub fn imp(&mut self) {
        self.additional_cycle = false;
        // This function should not be called
        assert_eq!(1, 0);
    }

    pub fn imm(&mut self) {
        let val = self.read_pc_byte();
        self.additional_cycle = false;
        self.fetched = val;
    }

    pub fn rel(&mut self) {
        self.addr_rel = self.read_pc_byte() as u16;

        if self.addr_rel & 0x80 != 0 {
            self.addr_rel |= 0xff00;
        }

        self.addr_abs = self.state.pc + self.addr_rel;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn abs(&mut self) {
        self.addr_abs = self.read_pc_word_le();
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn abx(&mut self) {
        let temp = self.read_pc_word_le();
        self.addr_abs = temp + self.state.x as u16;

        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = Cpu::same_page(self.addr_abs, temp);
    }

    pub fn aby(&mut self) {
        let temp = self.read_pc_word_le();

        self.addr_abs = temp + self.state.y as u16;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = Cpu::same_page(self.addr_abs, temp);
    }

    pub fn ind(&mut self) {
        let ptr = self.read_pc_word_le();

        let left: u16;
        // simulate a hardware bug on page boundary case
        if ptr & 0x00ff != 0 {
            left = ptr & 0xff00;
        } else {
            left = ptr + 1;
        }

        let lo = self.read_bus(&ptr) as u16;
        let hi = self.read_bus(&left) as u16;

        self.addr_abs = hi << 8 | lo;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn zp0(&mut self) {
        let temp = self.read_pc_byte() as u16;
        self.addr_abs = temp & 0x00ff;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn zpx(&mut self) {
        let mut temp = self.read_pc_byte() as u16;
        temp += self.state.x as u16;
        self.addr_abs = temp & 0x00ff;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn zpy(&mut self) {
        let mut temp = self.read_pc_byte() as u16;
        temp += self.state.y as u16;
        self.addr_abs = temp & 0x00ff;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    // for the x register
    pub fn exir(&mut self) {
        let val = self.read_pc_byte() as u16;

        let lo_addr = (val + self.state.x as u16) & 0x00ff;
        let lo = self.read_bus(&lo_addr) as u16;

        let hi_addr = (val + self.state.x as u16 + 1) & 0x00ff;
        let hi = self.read_bus(&hi_addr) as u16;

        self.addr_abs = (hi << 8) | lo;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn irex(&mut self) {
        let val = self.read_pc_byte() as u16;
        let lo_addr = val & 0x00ff;
        let lo = self.read_bus(&lo_addr) as u16;

        let hi_addr = (val + 1) & 0x00ff;
        let hi = self.read_bus(&hi_addr) as u16;

        let temp = (hi << 8) | lo;
        self.addr_abs = temp + self.state.y as u16;
        self.fetched = self.read_bus(&self.addr_abs);

        self.additional_cycle = Cpu::same_page(self.addr_abs, temp);
    }
}

pub mod mos6502_addressing_modes {
    #[derive(Copy, Clone)]
    pub enum AddrMode {
        Accumulator,
        Implied,
        Immediate,
        Relative,
        Absolute,
        Indirect,
        ZeroPage,
        ZeroPageX,
        ZeroPageY,
        IndexedX,
        IndexedY,
        IndexedIndirect,
        IndirectIndexed,
    }
    impl std::fmt::Debug for AddrMode {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            use self::AddrMode::*;
            let which = match *self {
                Accumulator => "Accumulator AM",
                Implied => "Implied AM",
                Immediate => "Immediate AM",
                Indirect => "Indirect AM",
                Relative => "Relative AM",
                Absolute => "Absolute AM",
                ZeroPage => "ZeroPage AM",
                ZeroPageX => "ZeroPageX AM",
                ZeroPageY => "ZeroPageY AM",
                IndexedX => "IndexedX AM",
                IndexedY => "IndexedY AM",
                IndexedIndirect => "IndexedIndirect AM",
                IndirectIndexed => "IndirectIndexed AM",
            };
            f.write_str(&which)
        }
    }
}

#[test]
fn cpu_read() {
    let mos6502 = Cpu::new();
    let addr = 0x4151;
    let value_original = 0;
    let value_read = mos6502.read_bus(&addr);
    assert_eq!(value_original, value_read);
}

#[cfg(test)]
mod addr_modes_test {
    use super::*;

    #[test]
    fn cpu_rel_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_imp_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_imm_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_abs_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_absx_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_absy_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_zp0_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_zpx_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_zpy_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_acc_am() {
        let mut mos6502 = Cpu::new();
        let i = instr("Using ACC AM", 0x00, Cpu::rol, AddrMode::Accumulator, 1, 2);
        println!("{:?}", i);
        mos6502.current = Some(i);
        mos6502.tick();
    }

    #[test]
    fn cpu_ind_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_exir_am() {
        let mos6502 = Cpu::new();
    }

    #[test]
    fn cpu_irex_am() {
        let mos6502 = Cpu::new();
    }
}
