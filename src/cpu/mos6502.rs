#![cfg_attr(debug_assertions, allow(dead_code, unused_imports, unused_variables))]
use crate::bus::dummy_bus::*;

use self::mos6502_addressing_modes::AddrMode;

use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
    // TODO: 'busline' doesn't really need the Option wrapper because there is only one main bus
    // which is directly connected to the cpu and only one is needed.
    busline: Option<DummyMainBus>,
    actual_addr: u16,
    time: u64,
    current: Option<Instruction>,
    state: RegSet,
}

impl Cpu {
    fn new() -> Self {
        Self {
            busline: Some(DummyMainBus::new_test()),
            actual_addr: 0,
            time: 0,
            current: None,
            state: RegSet::new(),
        }
    }

    fn read_bus(&self, addr: &u16) -> u8 {
        let mut val: u8 = 0;
        if let Some(bus) = &self.busline {
            // let bus = bus.borrow();
            val = bus.read(addr);
        }
        val
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
        let opcode = self.read_bus(&prev);
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

    fn locate(&self, addr_info: &[&u16], mode: self::mos6502_addressing_modes::AddrMode) {
        use self::mos6502_addressing_modes::{AddrMode::*, *};
        let addr = addr_info[0];

        let res = match mode {
            Accumulator => self.acc(),
            Implied => self.imp(),
            Immediate => self.imm(addr),
            Relative =>  {
                let mut rel = 0;
                if addr_info.len() > 1 {
                    rel = *addr_info[1];
                }
                // self.rel(addr, &rel)
                None
            }, 
            Absolute => self.abs(addr),
            ZeroPage => self.zp0(addr),
            _ => None
            // ZeroPageX => self.zpx(addr),
            // ZeroPageY => self.zpy(addr),
            // IndexedX => self.abx(addr),
            // IndexedY => self.aby(addr),
            // IndexedIndirect => self.exir(addr),
            // Indirect => self.ind(addr)
            // IndirectIndexed => self.irex(addr),
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
         0x00 => instr("TEST", 0x00, mos6502_instruction_set::nop, AddrMode::Accumulator, 0, 0),
    };
}

mod mos6502_instruction_set {
    use super::*;

    pub fn nop(cpu: &mut Cpu) {}
}

impl Cpu {
    pub fn acc(&self) -> Option<u16> {
        None
    }

    pub fn imp(&self) -> Option<u16> {
        None
    }

    pub fn imm(&self, addr: &u16) -> Option<u16> {
        None
    }

    pub fn rel(&self, addr: &u16, rel: &u16) -> Option<u16> {
        let mut actual_addr: u16 = *addr;
        let mut rel = *rel;

        if rel & 0x80 != 0 {
            rel &= !0x80;
            actual_addr -= rel;
        } else {
            actual_addr += rel;
        }

        Some(actual_addr)
    }

    pub fn abs(&self, addr: &u16) -> Option<u16> {
        Some(*addr)
    }

    pub fn abx(&self, addr: &u16) -> Option<u16> {
        let actual_addr = *addr + self.state.x as u16;
        Some(actual_addr)
    }

    pub fn aby(&self, addr: &u16) -> Option<u16> {
        let actual_addr = *addr + self.state.y as u16;
        Some(actual_addr)
    }

    pub fn ind(&self, addr: &u16) -> Option<u16> {
        // Indirect is used only by JMP and the difference is that after the read, PC is set to the
        // value stored at this address. As no chages to the cpu state are done in the location()
        // method we do not have a different implementation then the one of abs().
        self.abs(addr)
    }

    pub fn zp0(&self, addr: &u16) -> Option<u16> {
        let lo = *addr & 0x00ff;
        Some(lo)
    }

    pub fn zpx(&self, addr: &u16) -> Option<u16> {
        let lo = (*addr & 0x00ff) + self.state.x as u16;
        Some(lo)
    }

    pub fn zpy(&self, addr: &u16) -> Option<u16> {
        let lo = (*addr & 0x00ff) + self.state.y as u16;
        Some(lo)
    }

    pub fn exir(&self, addr: &u16) -> Option<u16> {
        // for the x register
        let addr = (*addr & 0xff) as u8;
        let mut i = (addr + self.state.x) as u16;
        let lo = self.read_bus(&i) as u16; i += 1;
        let hi = self.read_bus(&i) as u16;
        let actual_addr = hi << 8 | lo;
        Some(actual_addr)
    }

    pub fn irex(&self, addr: &u16) -> Option<u16> {
        let mut i = *addr;
        let lo = self.read_bus(&i) as u16; i += 1;
        let hi = self.read_bus(&i) as u16;
        let mut actual_addr = hi << 8 | lo;
        actual_addr += self.state.y as u16;
        Some(actual_addr)
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
        let addr: u16 = 0x7f;
        let rel: u16 = 0xff;
        assert_eq!(mos6502.rel(&addr, &rel), Some(0));
    }

    #[test]
    fn cpu_imp_am() {
        let mos6502 = Cpu::new();
        let addr: u16 = 0x0ff0;
        assert_eq!(mos6502.imp(), None);
    }

    #[test]
    fn cpu_imm_am() {
        let mos6502 = Cpu::new();
        let addr: u16 = 0x01B2;
        assert_eq!(mos6502.imm(&addr), None);
    }

    #[test]
    fn cpu_abs_am() {
        let mos6502 = Cpu::new();
        let addr: u16 = 0xff10;
        assert_eq!(mos6502.abs(&addr), Some(0xff10));
    }

    #[test]
    fn cpu_absx_am() {
        let mut mos6502 = Cpu::new();
        mos6502.state.x = 4;
        let addr: u16 = 0xff06;
        assert_eq!(mos6502.abx(&addr), Some(0xff0A));
    }

    #[test]
    fn cpu_absy_am() {
        let mut mos6502 = Cpu::new();
        mos6502.state.y = 5;
        let addr: u16 = 0xff05;
        assert_eq!(mos6502.aby(&addr), Some(0xff0A));
    }

    #[test]
    fn cpu_zp0_am() {
        let mos6502 = Cpu::new();
        let addr: u16 = 0x0005;
        assert_eq!(mos6502.zp0(&addr), Some(5));
    }

    #[test]
    fn cpu_zpx_am() {
        let mut mos6502 = Cpu::new();
        mos6502.state.x = 3;
        let addr: u16 = 5;
        assert_eq!(mos6502.zpx(&addr), Some(8));
    }

    #[test]
    fn cpu_zpy_am() {
        let mut mos6502 = Cpu::new();
        mos6502.state.y = 5;
        let addr: u16 = 3;
        assert_eq!(mos6502.zpy(&addr), Some(8));
    }

    #[test]
    fn cpu_acc_am() {
        let mut mos6502 = Cpu::new();
        mos6502.state.a = 3;
        assert_eq!(mos6502.acc(), None);
    }

    #[test]
    fn cpu_ind_am() {
        let mos6502 = Cpu::new();
        let addr: u16 = 0x0ff0;
        assert_eq!(mos6502.ind(&addr), Some(0x0ff0));
    }

    #[test]
    fn cpu_exir_am() {
        let mut mos6502 = Cpu::new();
        mos6502.state.x = 0x4;
        let addr: u16 = 0x20;
        let val = mos6502.exir(&addr);
        assert_eq!(val, Some(0x2074));
    }

    #[test]
    fn cpu_irex_am() {
        let mut mos6502 = Cpu::new();
        mos6502.state.y = 0x10;
        let addr: u16 = 0x86;
        let val = mos6502.irex(&addr);
        assert_eq!(val, Some(0x4038));
    }

}

