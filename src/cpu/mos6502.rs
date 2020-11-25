#![cfg_attr(debug_assertions, allow(dead_code, unused_imports, unused_variables))]
use crate::bus::dummy_bus::*;

use self::mos6502_addressing_modes::AddrMode;

use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

type InstructionPtr = fn(&mut Cpu) -> ();

struct Instruction {
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
        // Self {
        //     mnemonic: self.mnemonic.clone(),
        //     opcode: self.opcode,
        //     am: self.am,
        //     cycles: self.cycles,
        //     bytes: self.bytes,
        //     f: self.f,
        // }
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
struct RegSet {
    a: u8,
    x: u8,
    y: u8,
    sp: u16,
    pc: u16,
    ps: u8,
}
impl RegSet {
    fn new() -> Self {
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
    // TODO: 'busline' doesn't really need the Option wrapper because there is only one main bus
    // which is directly connected to the cpu and only one is needed.
    busline: Option<DummyMainBus>,
    addr_abs: u16,
    addr_rel: u16,
    fetched: u8,
    time: u64,
    current: Option<Instruction>,
    state: RegSet,
    additional_cycle: bool,
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            busline: Some(DummyMainBus::new_test()),
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
        let prev = self.state.pc.clone();
        let opcode = self.read_bus(&prev);
        self.state.pc += 1;
        self.addr_abs = opcode as u16;
    }

    fn execute(&mut self) {
        // remove unwrap() (it panics - add default value)
        let instr = self.current.as_ref().unwrap().clone();
        self.locate(instr.am);
        let f = instr.f;
        f(self);
    }

    fn tick(&mut self) {
        self.fetch();
        self.decode();
        self.execute();
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

enum Vector {}
fn instr(
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

mod mos6502_addressing_modes {
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
        let mut mos6502 = Cpu::new();
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
        let mos6502 = Cpu::new();
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
