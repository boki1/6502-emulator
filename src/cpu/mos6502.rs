#![cfg_attr(debug_assertions, allow(dead_code, unused_imports, unused_variables))]

use std::collections::HashMap;
use std::{thread, time};

use ansi_term::Colour;
use lazy_static::lazy_static;
use log::*;

use crate::bus::dummy_bus::*;

use super::mos6502_dt::DECODING_TABLE;
use super::mos6502_iset::*;

use self::mos6502_addressing_modes::AddrMode;
use std::iter::FromIterator;

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
            ps: Flag::initial(),
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
        self.fetch();
        self.decode();
        self.execute();
    }

    pub fn reset(&mut self) {
        self.state.a = 0;
        self.state.x = 0;
        self.state.y = 0;

        self.state.ps = Flag::initial();
        self.state.pc = Vectors::RESET as u16;

        self.fetched = 0;

        self.addr_abs = 0;
        self.addr_rel = 0;
        self.temp_opcode = 0;

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

    fn irq(&mut self) {
        let skptr = self.state.sp;
        let mut pc_temp = self.state.pc;
        self.writ_word(&skptr, &pc_temp);
        let p = self.state.ps;
        self.writ_byte(&skptr, &p);
        self.state.sp = skptr;

        self.flag_drop(Flag::I);
        let mut load_addr = Vectors::IRQ as u16;
        let lo = self.read_and_inc(&mut load_addr) as u16;
        let hi = self.read_bus(&load_addr) as u16;
        pc_temp = hi << 8 | lo;
        self.state.pc = pc_temp;
    }

    fn nmi(&mut self) {
        let skptr = self.state.sp;
        let mut pc_temp = self.state.pc;
        self.writ_word(&skptr, &pc_temp);
        let p = self.state.ps;
        self.writ_byte(&skptr, &p);
        self.state.sp = skptr;

        self.flag_drop(Flag::I);
        let mut load_addr = Vectors::NMI as u16;
        let lo = self.read_and_inc(&mut load_addr) as u16;
        let hi = self.read_bus(&load_addr) as u16;
        pc_temp = hi << 8 | lo;
        self.state.pc = pc_temp;
    }

    pub fn flag(&self, f: Flag) -> bool {
        self.state.ps & f.as_num() != 0
    }

    pub fn flag_drop(&mut self, f: Flag) -> u8 {
        self.state.ps &= !f.as_num();
        self.state.ps
    }

    pub fn flag_raise(&mut self, f: Flag) -> u8 {
        self.state.ps |= f.as_num();
        self.state.ps
    }
}

const LOAD_ADDR_DEFAULT: u16 = 0x8000;

pub enum Vectors {
    NMI = 0xfffa,
    RESET = 0xfffc,
    IRQ = 0xfffe,
}

pub enum Flag {
    N = 1 << 7,
    V = 1 << 6,
    U = 1 << 5,
    B = 1 << 4,
    D = 1 << 3,
    I = 1 << 2,
    Z = 1 << 1,
    C = 1 << 0,
}

impl Flag {
    pub fn as_num(self) -> u8 {
        self as u8
    }

    pub fn initial() -> u8 {
        Flag::I.as_num() | Flag::U.as_num()
    }
}

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

impl Cpu {
    #[must_use]
    pub fn same_page(p: u16, q: u16) -> bool {
        (p & 0xFF00) == (q & 0xFF00)
    }

    pub fn disassemble_region(&self, begin: u16, end: u16) -> Asm {
        use AddrMode::*;

        let mut code_map: HashMap<u16, Box<String>> = HashMap::new();

        let mut line: String = String::new();
        let mut oper: &Instruction;
        let mut opcode: u8;
        let mut addr: u16 = begin;
        let mut rel_addr: u16;
        let mut temp_addr: u16 = addr;
        let mut entry: Option<&Instruction>;
        let mut val: u8;
        let mut full: u16;
        let mut spec: String = String::new();

        while addr < end {
            opcode = self.read_and_inc(&mut addr);
            entry = DECODING_TABLE.get(&opcode);
            if entry.is_none() {
                continue;
            }
            temp_addr = addr;
            oper = entry.unwrap();
            line = "".to_string();
            spec = "".to_string();
            let addr_str = temp_addr.to_string();
            line.push_str(&format!("{:#4x}:\t{}  ", temp_addr, oper.mnemonic));

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
                    rel_addr = addr + val as u16;
                    if rel_addr & 0x80 != 0 {
                        rel_addr -= 127;
                    }
                    spec = format!("{:#04x} [${:#4x}]", val, rel_addr);
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
            line.push_str(&format!(" {:?}", oper.am));

            code_map.insert(temp_addr, Box::from(line.clone()));
        }
        Asm::from(code_map, begin)
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
                Accumulator => "ACC",
                Implied => "IMP",
                Immediate => "IMM",
                Indirect => "IND", Relative => "REL",
                Absolute => "ABS",
                ZeroPage => "ZP0",
                ZeroPageX => "ZPX",
                ZeroPageY => "ZPY",
                IndexedX => "INX",
                IndexedY => "INY",
                IndexedIndirect => "EXIR",
                IndirectIndexed => "IREX",
            };
            f.write_str(&format!(" __{}", which))
        }
    }
}

#[derive(Debug)]
pub struct Asm {
    code: HashMap<u16, Box<String>>,
    origin: u16,
}

impl std::fmt::Display for Asm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t*={:#04x}\n", self.origin())?;
        for (key, val) in self.map().iter() {
            write!(f, "\t{}\n", val)?;
        }
        Ok(())
    }
}

impl Asm {
    pub fn from(code_map: HashMap<u16, Box<String>>, origin: u16) -> Self {
        let mut v: Vec<_> = code_map.into_iter().collect();
        v.sort_by(|x,y| x.0.cmp(&y.0));
        Self {
            code: HashMap::from_iter(v),
            origin,
        }
    }

    pub fn map_mut(&mut self) -> &mut HashMap<u16, Box<String>> {
        &mut self.code
    }

    pub fn map(&self) -> &HashMap<u16, Box<String>> {
        &self.code
    }

    pub fn map_sorted(&mut self) -> Vec<(&u16, &Box<String>)> {
        let mut sorted: Vec<(&u16, &Box<String>)> = self.map().iter().collect();
        sorted.sort_by(|a, b| a.0.cmp(b.0));
        sorted
    }

    pub fn origin(&self) -> u16 {
        self.origin.clone()
    }
}
