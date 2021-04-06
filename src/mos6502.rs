#![feature(concat_idents)]

use getset::{CopyGetters, Getters, MutGetters, Setters};
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) type Address = u16;
pub(crate) type Word = u16;
pub(crate) type Byte = u8;

/// This structure represents the registers each MOS 6502 has.
/// They include one 8-bit accumulator register (A), two 8-bit
/// index registers (X and Y), 7 1-bit processor status flag
/// bits (P), an 8-bit stack pointer (S), and a 16-bit program
/// counter (PC).
///
/// The status register (P) 7 holds the following data:
/// ---- ----\
/// NVss DIZC\
/// |||| ||||\
/// |||| |||+- Carry\
/// |||| ||+-- Zero\
/// |||| |+--- Interrupt Disable\
/// |||| +---- Decimal\
/// ||++------ No CPU effect, see: the B flag\
/// |+-------- Overflow\
/// +--------- Negative
///

/// **bit!** and **bit_setter!** - Generate a getter and a setter for a concrete bit of a field.
///
/// Example:
/// ```
/// #[inline]
/// fn carry(&self) -> bool {
///     (self.status & 1 << 7)) != 0;
/// }
///
/// #[inline]
/// fn set_carry(&self, value: bool) -> bool {
///     self.status = (self.status & !(1 << 7)) | ((value as u8) << 7);
/// }
/// ```

macro_rules! bit_setter {
    ($n: expr, $name: ident) => {
        #[inline]
        fn $name(&mut self, value: bool) {
            self.status = (self.status & !(1 << $n)) | ((value as u8) << $n);
        }
    };
}

macro_rules! bit {
    ($n: expr, $name: ident) => {
        #[inline]
        fn $name(&self) -> bool {
            (self.status & (1 << $n)) != 0
        }
    };
}

#[derive(Debug, Copy, Clone, PartialEq, Getters, CopyGetters, Setters, MutGetters)]
#[getset(get_copy = "pub", set = "pub", get_mut = "pub")]
pub struct RegisterSet {
    accumulator: i8,
    x_index: i8,
    y_index: i8,
    stk_ptr: u8,
    prog_counter: u16,
    status: u8,
}

impl RegisterSet {
    fn new() -> Self {
        Self {
            accumulator: 0,
            x_index: 0,
            y_index: 0,
            stk_ptr: 0xfd,
            prog_counter: 0,
            status: 0x24,
        }
    }

    bit!(0, carry);
    bit!(1, zero);
    bit!(2, irq_disabled);
    bit!(3, decimal_mode);
    bit!(4, brk);
    bit!(5, unused);
    bit!(6, overflowed);
    bit!(7, negative);

    bit_setter!(0, set_carry);
    bit_setter!(1, set_zero);
    bit_setter!(2, set_irq_disabled);
    bit_setter!(3, set_decimal_mode);
    bit_setter!(4, set_brk);
    bit_setter!(5, set_unused);
    bit_setter!(6, set_overflowed);
    bit_setter!(7, set_negative);
}

impl Default for RegisterSet {
    fn default() -> Self {
        RegisterSet::new()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Getters, CopyGetters, Setters, MutGetters, Default)]
#[getset(get_copy = "pub", set = "pub", get_mut = "pub")]
pub struct Timings {
    elapsed: u64,
    residual: u8,
}

#[derive(Debug, Copy, Clone, PartialEq, Getters, CopyGetters, Setters, MutGetters, Default)]
#[getset(get_copy = "pub", set = "pub", get_mut = "pub")]
pub struct InterruptHandling {
    pending_nmi: bool,
    pending_irq: bool,
}

// TODO:
// Dev doc
// Init properly
const NmiVector: u16 = 0x0000;
const IrqBrkVector: u16 = 0x0000;
const ResetVector: u16 = 0x0000;

pub struct Cpu {
    regset: RegisterSet,
    time: Timings,
    inter: InterruptHandling,
    bus_conn: Option<Rc<RefCell<dyn CommunicationInterface>>>,
}

impl Cpu {
    pub fn regset(&self) -> &RegisterSet {
        &self.regset
    }

    pub fn time(&self) -> &Timings {
        &self.time
    }

    pub fn interrupt_handles(&self) -> &InterruptHandling {
        &self.inter
    }
}

impl std::fmt::Debug for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Cpu {
    fn new() -> Self {
        Self {
            regset: RegisterSet::new(),
            time: Timings {
                elapsed: 0,
                residual: 0,
            },
            inter: InterruptHandling {
                pending_irq: false,
                pending_nmi: false,
            },
            bus_conn: None,
        }
    }

    fn cycle(&self) {}

    fn reset(&self) {}

    fn connect_to(&mut self, conn: Rc<RefCell<dyn CommunicationInterface>>) {
        if self.bus_conn.is_some() {
            return;
        }

        self.bus_conn = Some(conn);
    }
}

impl Cpu {
    fn read_byte(&self, address: Address) -> Byte {
        if let Some(bus) = &self.bus_conn {
            return (*bus.borrow()).read(address);
        }
        0
    }

    fn writ_byte(&self, address: Address, data: Byte) {
        if let Some(bus) = &self.bus_conn {
            return (*bus.borrow_mut()).write(address, data);
        }
    }

    fn read_word(&self, address: Address) -> Word {
        let lo = self.read_byte(address) as u16;
        let hi = self.read_byte(address + 1) as u16;
        (hi << 8) | lo
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Cpu::new()
    }
}

pub trait CommunicationInterface {
    fn read(&self, address: Address) -> Byte;
    fn write(&mut self, address: Address, data: Byte);
}

const RamSize: usize = 0x1fff;

/// The "host" of our cpu
/// Contains the contexual environment of the processor, most notably - memory.
pub struct MainBus {
    pub mem: Vec<Byte>,
}

impl MainBus {
    fn new() -> Self {
        Self {
            mem: vec![0x0; RamSize],
        }
    }
}

impl CommunicationInterface for MainBus {
    fn read(&self, address: Address) -> Byte {
        let addr = usize::from(address);
        if addr >= self.mem.len() {
            return 0;
        }
        self.mem[addr]
    }

    fn write(&mut self, address: Address, data: Byte) {
        let addr = usize::from(address);
        if addr >= self.mem.len() {
            return;
        }

        self.mem[addr] = data;
    }
}

/// Tests

#[test]
fn test__create_cpu() {
    let _ = Cpu::new();
    assert!(true);
}

#[test]
fn test__getters_fields() {
    let mut regset = RegisterSet::new();
    assert_eq!(regset.accumulator(), 0);
    assert_eq!(regset.stk_ptr(), 0xfd);
    regset.set_accumulator(1);
    assert_eq!(regset.accumulator(), 1);
}

#[test]
fn test__getters_and_setters_bits() {
    let mut regset = RegisterSet::default();
    let carry_is_set: bool = regset.carry();
    assert_eq!(carry_is_set, false);

    regset.set_carry(true);
    let carry_is_set: bool = regset.carry();
    assert_eq!(carry_is_set, true);
}

#[test]
fn test__create_bus() {
    let bus = MainBus::new();
    assert_eq!(bus.mem[0], 0x0);
}

#[test]
fn test__cpu_with_host() {
    let mut bus = MainBus::new();
    bus.write(1, 10);
    let mut cpu = Cpu::new();
    cpu.connect_to(Rc::new(RefCell::new(bus)));

    let retval1 = cpu.read_byte(1);
    let retval2 = cpu.read_byte(2);
    assert_eq!(retval1, 10);
    assert_eq!(retval2, 0);
}