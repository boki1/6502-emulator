#![cfg_attr(debug_assertions, allow(dead_code, unused_imports, unused_variables))]

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::iter::FromIterator;
use std::rc::Rc;

use lazy_static::lazy_static;
use log::*;

use crate::cart::cart::Cartridge;
use crate::nes::nes::*;

use super::mos6502_dt::DECODING_TABLE;
use super::mos6502_iset::*;

use self::mos6502_addressing_modes::AddrMode;

type InstructionPtr = fn(&mut Cpu) -> ();

/// Represents a single instruction
pub struct Instruction {
    pub mnemonic: &'static str,
    pub opcode: u16,
    pub am: AddrMode,
    pub cycles: u8,
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

    fn is_complete(&self) -> bool {
        self.cycles == 0
    }
}

#[derive(Debug, Copy, Clone)]
pub struct RegSet {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
    pub pc: u16,
    pub ps: u8,
}

/// Represents the MOS 6502's register set
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

/// The most central abstraction. Responsible for managing the CPU's status and instruction
/// execution. Stores some variables which are used in a lot of places in this implementation such
/// as `addr_abs` and `addr_rel`. The former contains the address of the operand that is in
/// `fetched`, and the latter makes the transition from relative addressing mode easier.
/// Also this CPU abstraction is considered as the owner of the main nes.
#[derive(Debug)]
pub struct Cpu {
    pub container: Option<Rc<RefCell<Nes>>>,
    pub addr_abs: u16,
    pub addr_rel: u16,
    temp_opcode: u8,
    pub fetched: u8,
    pub time: u64,
    pub current: Option<Instruction>,
    pub state: RegSet,
    pub additional_cycle: bool,
    temp_cycles: u8,
    instr_has_executed: bool,
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

impl NesComponent for Cpu {
    type NesConsole = Nes;
    type NesConsolePtr = Rc<RefCell<Self::NesConsole>>;

    fn attach_to(&mut self, container: Self::NesConsolePtr) {
        self.container = Some(container);
    }

    fn container_of(&self) -> &Self::NesConsole {
        unsafe { self.container.as_ref().unwrap().as_ptr().as_ref().unwrap() }
    }

    fn container_of_mut(&mut self) -> &mut Self::NesConsole {
        unsafe { self.container.as_mut().unwrap().as_ptr().as_mut().unwrap() }
    }
}

impl Cpu {
    pub fn new() -> Self {
        Self {
            container: None,
            addr_abs: 0,
            addr_rel: 0,
            temp_opcode: 0,
            fetched: 0,
            time: 0,
            temp_cycles: 0,
            additional_cycle: false,
            instr_has_executed: true,
            current: None,
            state: RegSet::new(),
        }
    }

    #[inline]
    pub fn instr_has_executed(&self) -> bool {
        self.instr_has_executed
    }

    /// Given a mutable reference to an address, make two consecutive reads and update the address
    /// respectively.
    pub fn read_word_and_inc(&self, addr: &mut u16) -> u16 {
        let lo = self.read_and_inc(addr);
        let hi = self.read_and_inc(addr);
        u16::from_le_bytes([lo, hi])
    }

    /// Given a mutable reference to an address, make a read and update the address.
    pub fn read_and_inc(&self, addr: &mut u16) -> u8 {
        let lo = self.read_bus(*addr);
        *addr += 1;
        lo
    }

    /// Read the byte pointed by the PC and update its value.
    pub fn read_pc_byte(&mut self) -> u8 {
        let temp = self.state.pc;
        self.state.pc += 1;
        self.read_bus(temp)
    }

    /// Read two consecutive bytes beginning from the one pointed by PC and update its value.
    /// NB: 6502 uses little-endian so the lower part of the word is stored before the higher.
    pub fn read_pc_word_le(&mut self) -> u16 {
        let lo = self.read_pc_byte();
        let hi = self.read_pc_byte();
        u16::from_le_bytes([lo, hi])
    }

    /// Given an address to read from make the read and return the value _without_ updating any
    /// other value.
    pub fn read_bus(&self, addr: u16) -> u8 {
        let cont = self.container_of();
        let val = cont.read(addr);
        val
    }

    /// Write a word of data (little-endian) to a given address.
    pub fn writ_word(&mut self, addr: &mut u16, val: u16) {
        let bytes = val.to_le().to_le_bytes();
        let val_lo = bytes[0];
        let val_hi = bytes[1];
        self.writ_byte(*addr, val_lo);
        *addr += 1;
        self.writ_byte(*addr, val_hi);

        // TODO: Fix
        // I saw this mistake too late.
        // *addr += 1;
    }

    /// Write a byte to a given address.
    pub fn writ_byte(&mut self, addr: u16, val: u8) {
        let cont = self.container_of_mut();
        cont.write(addr, val);
    }

    /// One of the four high-level functions about instruction operation: responsible for extracting the next instruction from
    /// memory and updating the program counter while also keeping the other status CPU fields
    /// up-to-date.
    fn fetch(&mut self) {
        info!("\tFetching...");
        let prev = self.state.pc.clone();
        info!("\tPC was {:#04x}", prev);
        self.temp_opcode = self.read_bus(prev);
        info!("\tNext instruction is {:#04x}", self.temp_opcode);
        self.state.pc += 1;
        info!("\tPC is {:#04x} now", self.temp_opcode);
    }

    /// One of the four high-level functions about instruction operation: responsible for mapping the fetched opcode in the
    /// previous step to a meaningful entry of the decoding table with type `Instruction`.
    fn decode(&mut self) {
        info!("Decoding...");
        info!("opcode={:?}", self.temp_opcode);
        if let Some(i) = DECODING_TABLE.get(&self.temp_opcode) {
            self.current = Some(i.clone());
            self.temp_cycles = i.cycles;
            info!("instruction present -> {:?}", i);
        } else {
            self.current = None;
            info!("instruction missing");
        }
    }

    /// One of the four high-level functions about instruction operation: responsible for calling the concrete addressing mode
    /// function and then execute the instruction itself.
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

    /// One of the high-level functions. Responsible for a _full instruction cycle_: fetch,
    /// decode and execute. Practically it is not a tick, but multiple ticks instead.
    pub fn tick(&mut self) {
        info!("Ticking...");

        if self.instr_has_executed() {
            self.fetch();
            self.decode();
            self.execute();
        }

        self.temp_cycles -= 1;
        self.instr_has_executed = if self.temp_cycles == 0 { true } else { false };
    }

    /// High-level function. Resets the status of the CPU, according to the provided information in
    /// the chip documentation. The CPU loads the vector from $fffc/$fffd into the program counter.
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
        self.temp_cycles = 0;

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

    /// Helper function. Given an addressing mode fetch the operand in `Cpu::field`.
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

    /// Handles IRQs
    /// The CPU pushes the program counter as well as the processor status onto the stack, disables
    /// interrupts and loads the vector from $fffe/$ffff (=Vectors::IRQ) into the program counter.
    fn irq(&mut self) {
        let mut skptr_u16 = self.state.sp as u16;
        let mut pc_temp = self.state.pc;
        self.writ_word(&mut skptr_u16, pc_temp);
        let p = self.state.ps;
        self.writ_byte(skptr_u16, p);
        self.state.sp = skptr_u16 as u8;

        self.flag_drop(Flag::I);
        let mut load_addr = Vectors::IRQ as u16;
        let lo = self.read_and_inc(&mut load_addr) as u16;
        let hi = self.read_bus(load_addr) as u16;
        pc_temp = hi << 8 | lo;
        self.state.pc = pc_temp;
    }

    /// Handles NMIs
    /// The CPU pushes the program counter as well as the processor status onto the stack, disables
    /// interrupts and loads the vector from $fffa/$fffb (=Vectors::NMI) into the program counter.
    fn nmi(&mut self) {
        let mut skptr_u16 = self.state.sp as u16;
        let mut pc_temp = self.state.pc;
        self.writ_word(&mut skptr_u16, pc_temp);
        let p = self.state.ps;
        self.writ_byte(skptr_u16, p);
        self.state.sp = skptr_u16 as u8;

        self.flag_drop(Flag::I);
        let mut load_addr = Vectors::NMI as u16;
        let lo = self.read_and_inc(&mut load_addr) as u16;
        let hi = self.read_bus(load_addr) as u16;
        pc_temp = hi << 8 | lo;
        self.state.pc = pc_temp;
    }

    /// Simple utility functions for dealing with flags.

    /// Checks whether the flag is set or not.
    pub fn flagv(&self, f: Flag) -> bool {
        self.state.ps & f.as_num() != 0
    }

    /// Sets or clears a given flag, according to the boolean parameter.
    pub fn flag(&mut self, f: Flag, m: bool) {
        if m {
            self.flag_raise(f);
        } else {
            self.flag_drop(f);
        }
    }

    /// Clears a flag.
    pub fn flag_drop(&mut self, f: Flag) -> u8 {
        self.state.ps &= !f.as_num();
        self.state.ps
    }

    /// Sets a flag.
    pub fn flag_raise(&mut self, f: Flag) -> u8 {
        self.state.ps |= f.as_num();
        self.state.ps
    }
}

/// Constant containing the address 0x8000 used as a default value of the reset vector.
const LOAD_ADDR_DEFAULT: u16 = 0x8000;

/// This value is assumed to be the base of the stack.
pub const STACK_BASE: u16 = 0x100;

/// The NMI, RESET and IRQ vectors.
/// When one of the following interrupts occur these vectors describe to which address the program
/// should continue.
pub enum Vectors {
    NMI = 0xfffa,
    RESET = 0xfffc,
    IRQ = 0xfffe,
}

/// The flags supported by the MOS 6502 in the P register.
/// N: negative
/// V: overflow
/// U: unused
/// B: TODO
/// D: decimal mode (unused in the NES)
/// I: interrupts disabled
/// Z: zero
/// C: carry
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
    /// Checks whether two addresses reside on the same page in memory.
    #[must_use]
    pub fn same_page(p: u16, q: u16) -> bool {
        (p & 0xFF00) == (q & 0xFF00)
    }

    /// Given a staring and ending address, disasseble the region.
    /// Returns a `Asm` structure.
    pub fn disassemble_region(&self, begin: u16, end: u16) -> Asm {
        use AddrMode::*;

        let mut code_map: HashMap<u16, Box<String>> = HashMap::new();

        let mut line: String;
        let mut oper: &Instruction;
        let mut opcode: u8;
        let mut addr: u16 = begin;
        let mut rel_addr: u16;
        let mut temp_addr: u16;
        let mut entry: Option<&Instruction>;
        let mut val: u8;
        let mut full: u16;
        let mut spec: String;

        let mut cols: u8;

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
            cols = 2;

            match oper.am {
                Accumulator => {}
                Implied => {}
                Immediate => {
                    val = self.read_and_inc(&mut addr);
                    spec = format!("#{:#02x}", val);
                    cols += 1;
                }
                Indirect => {
                    full = self.read_word_and_inc(&mut addr);
                    spec = format!("{:#04x}", full);
                    cols += 1;
                }
                Relative => {
                    val = self.read_and_inc(&mut addr);
                    rel_addr = addr;
                    if val > 127 {
                        rel_addr -= (0xff - val + 1) as u16;
                    } else {
                        rel_addr += val as u16;
                    }
                    spec = format!("{:#04x} (=${:#4x}) ", val, rel_addr);
                    cols += 4;
                }
                Absolute => {
                    full = self.read_word_and_inc(&mut addr);
                    spec = format!("{:#04x}", full);
                    cols += 1;
                }
                ZeroPage => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("{:#02x}", full);
                    cols += 1;
                }
                ZeroPageX => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("{:#02x}, X", full);
                    cols += 2;
                }
                ZeroPageY => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("{:#02x}, Y", full);
                    cols += 2;
                }
                IndexedX => {
                    full = self.read_word_and_inc(&mut addr);
                    spec = format!("{:#04x}, X", full);
                    cols += 2;
                }
                IndexedY => {
                    full = self.read_word_and_inc(&mut addr);
                    spec = format!("{:#04x}, Y", full);
                }
                IndexedIndirect => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("({:#02x}, X)", full);
                    cols += 2;
                }
                IndirectIndexed => {
                    full = self.read_and_inc(&mut addr) as u16;
                    spec = format!("({:#02x}), Y", full);
                    cols += 2;
                }
            }

            line.push_str(&spec);
            for i in 0..(7 - cols) {
                line.push_str("    ");
            }
            line.push_str(&format!("{:?}", oper.am));

            code_map.insert(temp_addr, Box::from(line.clone()));
        }
        Asm::from(code_map, begin)
    }
}

impl Cpu {
    /// The accumulator addressing mode.
    /// The operand is the A register.
    /// No additional cycles may occur.
    pub fn acc(&mut self) {
        info!("{:?}", AddrMode::Accumulator);
        self.fetched = self.state.a;
        self.additional_cycle = false;
    }

    /// The implied addressing mode.
    /// The operand is _implied_.
    /// No additional cycles may occur.
    pub fn imp(&mut self) {
        info!("{:?}", AddrMode::Implied);
        self.additional_cycle = false;
    }

    /// The immediate addressing mode.
    /// The operand is a 8-bit value. It is going to be used directly with the instruction without
    /// any memory read necessary.
    pub fn imm(&mut self) {
        info!("{:?}", AddrMode::Immediate);
        let val = self.read_pc_byte();
        self.additional_cycle = false;
        self.fetched = val;
    }

    /// The relative addressing mode.
    /// The operand is a value in the range (-128; 127). The PC is moved according to the given
    /// value.
    /// Here comes the usage of `addr_rel`.
    pub fn rel(&mut self) {
        info!("{:?}", AddrMode::Relative);
        self.addr_rel = self.read_pc_byte() as u16;

        if self.addr_rel & 0x80 != 0 {
            self.addr_rel |= 0xff00;
        }

        self.addr_abs = self.state.pc.wrapping_add(self.addr_rel);
        self.fetched = self.read_bus(self.addr_abs);
        self.additional_cycle = false;
    }

    /// The absolute addressing mode.
    /// The operand is a 16-bit address. It should be used to read from memory and extract the 16-bit
    /// address of the value which is to be used as a "CPU input".
    pub fn abs(&mut self) {
        info!("{:?}", AddrMode::Absolute);
        self.addr_abs = self.read_pc_word_le();
        self.fetched = self.read_bus(self.addr_abs);
        self.additional_cycle = false;
    }

    /// The absolute addressing mode with x offset.
    /// The operand is a 16-bit address. It should be used to read from memory and extract a 16-bit
    /// address which modified by x gives the address of the value which is to be used as a "CPU input".
    pub fn abx(&mut self) {
        info!("{:?}", AddrMode::IndexedX);
        let temp = self.read_pc_word_le();
        self.addr_abs = temp + self.state.x as u16;

        self.fetched = self.read_bus(self.addr_abs);
        self.additional_cycle = Cpu::same_page(self.addr_abs, temp);
    }

    /// The absolute addressing mode with y offset.
    /// The operand is a 16-bit address. It should be used to read from memory and extract a 16-bit
    /// address which modified by y gives the address of the value which is to be used as a "CPU input".
    pub fn aby(&mut self) {
        info!("{:?}", AddrMode::IndexedY);
        let temp = self.read_pc_word_le();

        self.addr_abs = temp + self.state.y as u16;
        self.fetched = self.read_bus(self.addr_abs);
        self.additional_cycle = Cpu::same_page(self.addr_abs, temp);
    }

    /// The indirect addressing mode.
    /// The JMP instruction is the only instruction that uses this addressing mode.
    /// The operand is 16-bit address which gets stored in the PC.
    /// Here appears a hardware bug. When having a page boundary crossed, the
    /// value wraps around.
    pub fn ind(&mut self) {
        info!("{:?}", AddrMode::Indirect);
        let ptr = self.read_pc_word_le();

        let left: u16;
        if ptr & 0x00ff != 0 {
            left = ptr & 0xff00;
        } else {
            left = ptr + 1;
        }

        let lo = self.read_bus(ptr) as u16;
        let hi = self.read_bus(left) as u16;

        self.addr_abs = hi << 8 | lo;
        self.fetched = self.read_bus(self.addr_abs);
        self.additional_cycle = false;
    }

    /// The zero-page addressing mode.
    /// Performs the same operation as the absolute, but limits the operand to reside in the first
    /// page.
    pub fn zp0(&mut self) {
        info!("{:?}", AddrMode::ZeroPage);
        let temp = self.read_pc_byte() as u16;
        self.addr_abs = temp & 0x00ff;
        self.fetched = self.read_bus(self.addr_abs);
        self.additional_cycle = false;
    }

    /// The zero-page with x addressing mode.
    /// Performs the same operation as the absolute with x offset, but limits the operand to reside in the first
    /// page.
    pub fn zpx(&mut self) {
        info!("{:?}", AddrMode::ZeroPageX);
        let mut temp = self.read_pc_byte() as u16;
        temp += self.state.x as u16;
        self.addr_abs = temp & 0x00ff;
        self.fetched = self.read_bus(self.addr_abs);
        self.additional_cycle = false;
    }

    /// The zero-page with y addressing mode.
    /// Performs the same operation as the absolute with y offset, but limits the operand to reside in the first
    /// page.
    pub fn zpy(&mut self) {
        info!("{:?}", AddrMode::ZeroPageY);
        let mut temp = self.read_pc_byte() as u16;
        temp += self.state.y as u16;
        self.addr_abs = temp & 0x00ff;
        self.fetched = self.read_bus(self.addr_abs);
        self.additional_cycle = false;
    }

    /// The indexed indirect addressing mode.
    /// The operand is a zero-page address. At this address is stored a another address which is
    /// the actual address of the value which is to be used as a CPU input.
    /// `addr_abs = (x + opcode[1]) & 0xFF`
    pub fn exir(&mut self) {
        info!("{:?}", AddrMode::IndexedIndirect);
        let val = self.read_pc_byte() as u16;

        let lo_addr = (val + self.state.x as u16) & 0x00ff;
        let lo = self.read_bus(lo_addr) as u16;

        let hi_addr = (val + self.state.x as u16 + 1) & 0x00ff;
        let hi = self.read_bus(hi_addr) as u16;

        self.addr_abs = (hi << 8) | lo;
        self.fetched = self.read_bus(self.addr_abs);
        self.additional_cycle = false;
    }

    /// The indirect indexed addressing mode.
    /// The CPU will first fetch the address stored at zero page location given as the operand.
    /// And then that address will be added to register Y to get the final target address.
    /// While indexed indirect addressing will only generate a zero-page address, the target
    /// address is not wrapped - it can be anywhere in the 16-bit address space.
    pub fn irex(&mut self) {
        info!("{:?}", AddrMode::IndirectIndexed);
        let val = self.read_pc_byte() as u16;
        let lo_addr = val & 0x00ff;
        let lo = self.read_bus(lo_addr) as u16;

        let hi_addr = (val + 1) & 0x00ff;
        let hi = self.read_bus(hi_addr) as u16;

        let temp = (hi << 8) | lo;
        self.addr_abs = temp + self.state.y as u16;
        self.fetched = self.read_bus(self.addr_abs);

        self.additional_cycle = Cpu::same_page(self.addr_abs, temp);
    }
}

pub mod mos6502_addressing_modes {
    /// Here are described all addressing modes.
    #[derive(Copy, Clone, PartialEq)]
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
                Indirect => "IND",
                Relative => "REL",
                Absolute => "ABS",
                ZeroPage => "ZP0",
                ZeroPageX => "ZPX",
                ZeroPageY => "ZPY",
                IndexedX => "INX",
                IndexedY => "INY",
                IndexedIndirect => "EXIR",
                IndirectIndexed => "IREX",
            };
            f.write_str(&format!(" `{}`", which))
        }
    }
}

/// Used for disassembly. Stores the beginning address of the code and a hashmap of the addresses
/// and string representation of the line.
#[derive(Debug)]
pub struct Asm {
    code: HashMap<u16, Box<String>>,
    origin: u16,
}

impl std::fmt::Display for Asm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\t*={:#04x}\n", self.origin())?;
        for (key, val) in self.this_map().iter() {
            write!(f, "\t{}\n", val)?;
        }
        Ok(())
    }
}

impl Asm {
    pub fn from(code_map: HashMap<u16, Box<String>>, origin: u16) -> Self {
        let mut v: Vec<_> = code_map.into_iter().collect();
        v.sort_by(|x, y| x.0.cmp(&y.0));
        Self {
            code: HashMap::from_iter(v),
            origin,
        }
    }

    pub fn map_mut(&mut self) -> &mut HashMap<u16, Box<String>> {
        &mut self.code
    }

    pub fn this_map(&self) -> &HashMap<u16, Box<String>> {
        &self.code
    }

    pub fn map_sorted(&self) -> Vec<(&u16, &Box<String>)> {
        let mut sorted: Vec<(&u16, &Box<String>)> = self.this_map().iter().collect();
        sorted.sort_by(|a, b| a.0.cmp(b.0));
        sorted
    }

    pub fn origin(&self) -> u16 {
        self.origin.clone()
    }
}
