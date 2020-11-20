#![cfg_attr(debug_assertions, allow(dead_code, unused_imports, unused_variables))]
use crate::bus::dummy_bus::*;

use self::mos6502_addressing_modes::AddrMode;

use lazy_static::lazy_static;
use std::collections::HashMap;

type InstructionPtr = fn(&mut Cpu) -> ();

struct Instruction {
    mnemonic: String,
    opcode: u16,
    am: AddrMode,
    cycles: u8,
    bytes: u8,
    f: InstructionPtr,
}

impl Clone for Instruction {
    fn clone(&self) -> Self {
        Self {
            mnemonic: self.mnemonic.clone(),
            opcode: self.opcode,
            am: self.am,
            cycles: self.cycles,
            bytes: self.bytes,
            f: self.f,
        }
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
        _mnemonic: &str,
        opcode: u16,
        am: AddrMode,
        cycles: u8,
        bytes: u8,
        f: InstructionPtr,
    ) -> Self {
        Instruction {
            mnemonic: _mnemonic.to_string(),
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
    busline: Option<&'static DummyMainBus>,
    actual_addr: u16,
    time: u64,
    current: Option<Instruction>,
    state: RegSet,
}

impl Cpu {
    fn new() -> Self {
        Self {
            busline: None,
            actual_addr: 0,
            time: 0,
            current: None,
            state: RegSet::new(),
        }
    }

    fn read_wrapper(&self, addr: &u16) -> u8 {
        let mut val: u8 = 0;
        if let Some(bus) = &self.busline {
            val = bus.read(addr);
        }
        val
    }

    fn write_wrapper(&mut self, addr: &u16, val: &u8) {
        if let Some(holder) = &self.busline {
            //     let mut inner = *holder;
            //     inner.write(addr, val);
        }
    }

    fn decode(&mut self) {
        // Kind of ugly.
        // todo: Clean in up, please.

        let opcode = &self.actual_addr;
        if let Some(i) = DECODING_TABLE.get(opcode) {
            self.current = Some(i.clone());
        } else {
            self.current = None;
        }
    }

    fn fetch(&mut self) {
        let prev = self.state.pc.clone();
        let opcode = self.read_wrapper(&prev);
        self.state.pc += 1;
        self.actual_addr = opcode as u16;
    }

    fn execute(&mut self) {}

    fn tick(&mut self) {
        {
            self.fetch();
        }
        {
            self.decode();
        }
        {
            self.execute();
        }
    }

    fn locate(&self, addr: &u16, mode: self::mos6502_addressing_modes::AddrMode) {
        use self::mos6502_addressing_modes::{AddrMode::*, *};

        let res = match mode {
            Accumulator => acc(),
            Implied => imp(),
            Immediate => imm(),
            Relative => rel(),
            Absolute => abs(),
            ZeroPage => zp0(),
            ZeroPageX => zpx(),
            ZeroPageY => zpy(),
            IndexedX => abx(),
            IndexedY => aby(),
            IndexedIndirect => exir(),
            IndirectIndexed => irex(),
        };
    }
}

enum Vector {}
fn instr(
    mnemonic: &str,
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
         0x00 => instr("TEST", 0x00, mos6502_instruction_set::nop, AddrMode::Accumulator, 0, 0)
        //      let t map = HashMap::new();
        // map.insert("James", vec!["user", "admin"]);
        // map.insert("Jim", vec!["user"]);
        // map
    };
}

mod mos6502_instruction_set {
    use super::*;

    pub fn nop(cpu: &mut Cpu) {}
}

mod mos6502_addressing_modes {
    #[derive(Copy, Clone)]
    pub enum AddrMode {
        Accumulator,
        Implied,
        Immediate,
        Relative,
        Absolute,
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
    pub fn acc() {}
    pub fn imp() {}
    pub fn imm() {}
    pub fn rel() {}
    pub fn abs() {}
    pub fn abx() {}
    pub fn aby() {}
    pub fn zp0() {}
    pub fn zpx() {}
    pub fn zpy() {}
    pub fn exir() {}
    pub fn irex() {}
}

#[test]
fn cpu_init() {
    let _mos6502 = Cpu::new();
    assert_eq!(1, 1);
}

#[test]
fn cpu_write_and_read() {
    let mut mos6502 = Cpu::new();
    let addr = 0x4151;
    let value_original = 4;
    mos6502.write_wrapper(&addr, &value_original);
    let value_read = mos6502.read_wrapper(&addr);
    // assert_eq!(value_original, value_read);
}
