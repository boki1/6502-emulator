#![cfg_attr(debug_assertions, allow(dead_code, unused_imports, unused_variables))]

use std::{thread, time};
use std::collections::HashMap;

use ansi_term::Colour;
use lazy_static::lazy_static;
use log::*;

use crate::bus::dummy_bus::*;

use super::mos6502_dt::DECODING_TABLE;
use super::mos6502_iset::*;

use self::mos6502_addressing_modes::AddrMode;

type InstructionPtr = fn(&mut Cpu) -> ();

pub struct Instruction {
    mnemonic: &'static str,
    pub opcode: u16,
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
            .field("addrm", &self.am)
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
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u16,
    pub pc: u16,
    pub ps: u8,
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

impl std::fmt::Display for RegSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "A=${:#4x}\nX=${:#4x} | Y=${:#4x}\nP={:#8b}\nSP=${:#4x}\nPC=${:#4x}\n",
            self.a, self.x, self.y, self.ps, self.sp, self.pc
        )
    }
}

#[derive(Debug)]
pub struct Cpu {
    pub busline: Option<DummyMainBus>,
    addr_abs: u16,
    addr_rel: u16,
    temp_opcode: u8,
    pub fetched: u8,
    pub time: u64,
    pub current: Option<Instruction>,
    pub state: RegSet,
    additional_cycle: bool,
}

impl std::fmt::Display for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "T={:#4x}\n{}AA={:#4x}\nAR={:#4x}\n",
            self.time, self.state, self.addr_abs, self.addr_rel
        )
    }
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            busline: Some(DummyMainBus::new()),
            addr_abs: 0,
            addr_rel: 0,
            temp_opcode: 0,
            fetched: 0,
            time: 0,
            additional_cycle: false,
            current: None,
            state: RegSet::new(),
        }
    }

    pub fn read_word_and_inc(&self, addr: &mut u16) -> u16 {
        let lo = self.read_and_inc(addr);
        let hi = self.read_and_inc(addr);
        u16::from_le_bytes([lo, hi])
    }

    pub fn read_and_inc(&self, addr: &mut u16) -> u8 {
        let lo = self.read_bus(addr);
        *addr += 1;
        lo
    }

    pub fn read_pc_byte(&mut self) -> u8 {
        let temp = self.state.pc;
        self.state.pc += 1;
        self.read_bus(&temp)
    }

    pub fn read_pc_word_le(&mut self) -> u16 {
        let lo = self.read_pc_byte();
        let hi = self.read_pc_byte();
        u16::from_le_bytes([lo, hi])
    }

    pub fn read_bus(&self, addr: &u16) -> u8 {
        let mut val: u8 = 0;
        if let Some(bus) = &self.busline {
            val = bus.read(addr);
        }
        val
    }

    pub fn writ_word(&mut self, addr: &u16, val: &u16) -> bool {
        let bytes = val.to_le().to_le_bytes();
        let val_lo = bytes[0];
        let val_hi = bytes[1];
        let mut addr_ = *addr;
        if self.writ_byte(&addr_, &val_lo) == false {
            return false;
        }
        addr_ += 1;
        if self.writ_byte(&addr_, &val_hi) == false {
            return false;
        }

        true
    }

    pub fn writ_byte(&mut self, addr: &u16, val: &u8) -> bool {
        let bus = &mut self.busline;

        if bus.is_none() {
            return false;
        }

        let bus = bus.as_mut().unwrap();
        bus.write(addr, val);

        return true;
    }

    fn fetch(&mut self) {
        info!("\tFetching...");
        let prev = self.state.pc.clone();
        info!("\tPC was {:#04x}", prev);
        self.temp_opcode = self.read_bus(&prev);
        info!("\tNext instruction is {:#04x}", self.temp_opcode);
        self.state.pc += 1;
        info!("\tPC is {:#04x} now", self.temp_opcode);
    }

    fn decode(&mut self) {
        info!("Decoding...");
        info!("opcode={:?}", self.temp_opcode);
        if let Some(i) = DECODING_TABLE.get(&self.temp_opcode) {
            self.current = Some(i.clone());
            info!("instruction present -> {:?}", i);
        } else {
            self.current = None;
            info!("instruction missing");
        }
    }

    fn execute(&mut self) {
        info!("Executing...");

        let i: Instruction = match self.current.as_ref() {
            Some(ii) => self.current.as_ref().unwrap().clone(),
            None => instr("BLANK", 0x00, Cpu::nop, AddrMode::Implied, 1, 1),
        };

        info!("\t{:?}", i);
        self.locate(i.am);
        let f = i.f;
        f(self);
    }

    pub fn tick(&mut self) {
        info!("Ticking...");
        // thread::sleep(time::Duration::from_secs(1));
        self.fetch();
        // thread::sleep(time::Duration::from_secs(1));
        self.decode();
        // thread::sleep(time::Duration::from_secs(1));
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
        self.temp_opcode = 0;

        self.state.pc = Vectors::RESET as u16;
        let mut temp_pc = self.state.pc.clone();
        let __pc = self.read_word_and_inc(&mut temp_pc);
        if __pc == 0 {
            self.state.pc = LOAD_ADDR_DEFAULT;
        } else {
            self.state.pc = __pc;
        }

        info!("Reseting...");
        info!("{}", self);
    }

    fn locate(&mut self, mode: self::mos6502_addressing_modes::AddrMode) {
        info!("Locating...");
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

const LOAD_ADDR_DEFAULT: u16 = 0x8000;

pub enum Vectors {
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

mod mos6502_instruction_set {
    use super::*;

    pub fn nop(cpu: &mut Cpu) {}
}

impl Cpu {
    #[must_use]
    pub fn same_page(p: u16, q: u16) -> bool {
        (p & 0xFF00) == (q & 0xFF00)
    }

    pub fn disasemble_region(&self, begin: u16, end: u16) {
        //-> Asm {
        use AddrMode::*;

        let _code_map: HashMap<u16, &str> = HashMap::new();

        let mut line: String = String::new();
        let mut oper: &Instruction;
        let mut opcode: u8;
        let mut addr: u16 = begin;
        let mut _temp_addr: u16 = addr;
        let mut entry: Option<&Instruction>;
        let mut val: u8;
        let mut full: u16;
        let mut spec: String = String::new();

        for i in begin..end {
            opcode = self.read_and_inc(&mut addr);
            entry = DECODING_TABLE.get(&opcode);
            if entry.is_none() {
                continue;
            }
            _temp_addr = addr;
            oper = entry.unwrap();
            line.clear();
            spec.clear();
            let addr_str = _temp_addr.to_string();
            line.push_str(&format!("{:#4x}: \t{} \t", _temp_addr, oper.mnemonic));

            match oper.am {
                Accumulator => {}
                Implied => {}
                Immediate => {
                    val = self.read_and_inc(&mut addr);
                    spec = format!("#{:#02x}", val);
                }
                Indirect => {
                    full = self.read_word_and_inc(&mut addr);
                    spec = format!("{:#04x}", full);
                }
                Relative => {
                    val = self.read_and_inc(&mut addr);
                    spec = format!("{:#04x} [${:#4x}]", val, addr);
                }
                Absolute => {
                    full = self.read_word_and_inc(&mut addr);
                    spec = format!("{:#04x}", full);
                }
                ZeroPage => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("{:#02x}", full);
                }
                ZeroPageX => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("{:#02x}, X", full);
                }
                ZeroPageY => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("{:#02x}, Y", full);
                }
                IndexedX => {
                    full = self.read_word_and_inc(&mut addr);
                    spec = format!("{:#04x}, X", full);
                }
                IndexedY => {
                    full = self.read_word_and_inc(&mut addr);
                    spec = format!("{:#04x}, Y", full);
                }
                IndexedIndirect => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("({:#02x}, X)", full);
                }
                IndirectIndexed => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("({:#02x}), Y", full);
                }
            }

            line.push_str(&spec);
            // line.push_str(&format!(" {:?}", oper.am));

            println!("{}", line);
            // code_map.insert(temp_addr, &a);
        }
        // Asm::from(code_map, begin)
    }
}

impl Cpu {
    pub fn acc(&mut self) {
        info!("{:?}", AddrMode::Accumulator);
        self.fetched = self.state.a;
        self.additional_cycle = false;
    }

    pub fn imp(&mut self) {
        info!("{:?}", AddrMode::Implied);
        self.additional_cycle = false;
        // This function should not be called
        // TODO: Uncomment next assert
        // assert_eq!(1, 0);
    }

    pub fn imm(&mut self) {
        info!("{:?}", AddrMode::Immediate);
        let val = self.read_pc_byte();
        self.additional_cycle = false;
        self.fetched = val;
    }

    pub fn rel(&mut self) {
        info!("{:?}", AddrMode::Relative);
        self.addr_rel = self.read_pc_byte() as u16;

        if self.addr_rel & 0x80 != 0 {
            self.addr_rel |= 0xff00;
        }

        self.addr_abs = self.state.pc.wrapping_add(self.addr_rel);
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn abs(&mut self) {
        info!("{:?}", AddrMode::Absolute);
        self.addr_abs = self.read_pc_word_le();
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn abx(&mut self) {
        info!("{:?}", AddrMode::IndexedX);
        let temp = self.read_pc_word_le();
        self.addr_abs = temp + self.state.x as u16;

        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = Cpu::same_page(self.addr_abs, temp);
    }

    pub fn aby(&mut self) {
        info!("{:?}", AddrMode::IndexedY);
        let temp = self.read_pc_word_le();

        self.addr_abs = temp + self.state.y as u16;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = Cpu::same_page(self.addr_abs, temp);
    }

    pub fn ind(&mut self) {
        info!("{:?}", AddrMode::Indirect);
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
        info!("{:?}", AddrMode::ZeroPage);
        let temp = self.read_pc_byte() as u16;
        self.addr_abs = temp & 0x00ff;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn zpx(&mut self) {
        info!("{:?}", AddrMode::ZeroPageX);
        let mut temp = self.read_pc_byte() as u16;
        temp += self.state.x as u16;
        self.addr_abs = temp & 0x00ff;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    pub fn zpy(&mut self) {
        info!("{:?}", AddrMode::ZeroPageY);
        let mut temp = self.read_pc_byte() as u16;
        temp += self.state.y as u16;
        self.addr_abs = temp & 0x00ff;
        self.fetched = self.read_bus(&self.addr_abs);
        self.additional_cycle = false;
    }

    // for the x register
    pub fn exir(&mut self) {
        info!("{:?}", AddrMode::IndexedIndirect);
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
        info!("{:?}", AddrMode::IndirectIndexed);
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

#[derive(Debug)]
pub struct Asm<'a> {
    code: HashMap<u16, &'a str>,
    origin: u16,
}

impl std::fmt::Display for Asm<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t*={:#04x}\n", self.origin())?;
        for (key, val) in self.map().iter() {
            write!(f, "\t{}\n", val)?;
        }
        Ok(())
    }
}

impl<'a> Asm<'a> {
    pub fn from(code_map: HashMap<u16, &'a str>, origin: u16) -> Self {
        Self {
            code: code_map,
            origin,
        }
    }

    pub fn map_mut(&mut self) -> &mut HashMap<u16, &'a str> {
        &mut self.code
    }

    pub fn map(&self) -> &HashMap<u16, &'a str> {
        &self.code
    }

    pub fn origin(&self) -> u16 {
        self.origin.clone()
    }
}
