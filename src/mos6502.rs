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

impl Timings {
    fn next(&mut self) {
        *self.residual_mut() -= 1;
        *self.elapsed_mut() += 1;
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Getters, CopyGetters, Setters, MutGetters, Default)]
#[getset(get_copy = "pub", set = "pub", get_mut = "pub")]
pub struct InterruptHandling {
    pending_nmi: bool,
    pending_irq: bool,
}

/// **Jump vectors**
/// In any of this exception cases - NMI, IRQ, BRK or a RESET, the PC should jump to the
/// concrete address, also called _vector_.
const NMI_VECTOR: Address = 0xfffa;
const RESET_VECTOR: Address = 0xfffc;
const IRQ_BRK_VECTOR: Address = 0xfffe;

pub struct Cpu {
    regset: RegisterSet,
    time: Timings,
    inter: InterruptHandling,
    bus_conn: Option<Rc<RefCell<dyn CommunicationInterface>>>,
}

impl Cpu {
    pub fn regset_mut(&mut self) -> &mut RegisterSet {
        &mut self.regset
    }

    pub fn regset(&self) -> &RegisterSet {
        &self.regset
    }

    pub fn time_mut(&mut self) -> &mut Timings {
        &mut self.time
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
                residual: 0
            },
            inter: InterruptHandling {
                pending_irq: false,
                pending_nmi: false,
            },
            bus_conn: None,
        }
    }

    /// **new_connected()** - Creates a new instance of a cpu with a bus interface
    /// supported
    fn new_connected(bus_conn: Option<Rc<RefCell<dyn CommunicationInterface>>>) -> Self {
        Self {
            bus_conn,
            ..Cpu::new()
        }
    }

    fn cycle(&mut self) {
        // if !self.time.residual() {}

        self.time_mut().next();
    }

    /// **inthandle()** - Handles any interrupts of the cpu.
    /// The different kinds of intterrupts which the MOST 6502 supports
    /// are BRK (software interrupt), IRQ (interrupt request) and
    /// NMI (non-maskable interrupt).
    /// NMIs cannot be disabled.
    /// In order to allow IRQs, the flag `irq_disabled` in the status
    /// register has to be clear.
    fn inthandle(&mut self) -> bool {
        let nmi_flag: bool = self.interrupt_handles().pending_nmi();
        let mut irq_flag: bool = self.interrupt_handles().pending_irq();
        irq_flag &= !self.regset().irq_disabled();

        let is_interrupted = nmi_flag || irq_flag;

        if !is_interrupted {
            return false;
        }

        return true;
    }

    /// **reset()** - Performs a reset to the internal state of the cpu
    /// Among the flags in the status register, only the *UNUSED* flag is set.
    /// Reset takes some time, so 8 cycles are set to be remaining in the
    /// internal "clock" of the cpu
    fn reset(&mut self) {
        let mut regset = RegisterSet::new();
        regset.set_status(0x00);
        regset.set_unused(true);
        self.regset = regset;

        let timing = Timings { residual: 8, elapsed: 0};
        self.time = timing;
    }

    /// **connect()** - Connects the cpu to a bus, providing a context
    /// for read and write operations.
    fn connect_to(&mut self, conn: Rc<RefCell<dyn CommunicationInterface>>) {
        if self.bus_conn.is_some() {
            return;
        }

        self.bus_conn = Some(conn);
    }
}

impl Cpu {


    /// **read_byte()** - Initiates a read request to the interface
    /// **if one is present**
    fn read_byte(&self, address: Address) -> Byte {
        if let Some(bus) = &self.bus_conn {
            if let Some(data) = (*bus.borrow()).read(address) {
                return data;
            }
        }
        0
    }

    /// **writ_byte()** - Initiates a write request to the interface
    /// **if one is present**
    fn writ_byte(&self, address: Address, data: Byte) {
        if let Some(bus) = &self.bus_conn {
            return (*bus.borrow_mut()).write(address, data);
        }
    }

    /// **read_word()** - Wrapper function for reading two sequential
    /// bytes from the interface **if one is present**.
    fn read_word(&self, address: Address) -> Word {
        let lo = self.read_byte(address);
        let hi = self.read_byte(address + 1);
        Word::from_le_bytes([lo, hi])
    }

    /// **read_some()** - Reads sequence of bytes from the interface
    fn read_some(&self, address: Address, len: u16) -> Vec<Byte> {
        if let Some(bus) = &self.bus_conn {
            if let Some(result) = (*bus.borrow()).read_seq(address, len) {
                return result;
            }
        }

        vec![]
    }
}

impl Default for Cpu {
    fn default() -> Self {
        Cpu::new()
    }
}

pub trait CommunicationInterface {
    /// **read()** - Read the value of a given address from the interface
    fn read(&self, address: Address) -> Option<Byte>;

    /// **write()** - Write a value to a given address of the interface
    fn write(&mut self, address: Address, data: Byte);

    /// **read_seq()** - Read sequental from `address` to `address + len`
    /// (or less if the limit is exceeded)
    fn read_seq(&self, address: Address, len: u16) -> Option<Vec<Byte>>;
}

const RAM_SIZE: usize = 0x07ff;

/// The "host" of our cpu
/// Contains the contexual environment of the processor, most notably - memory.
pub struct MainBus {
    pub mem: Vec<Byte>,
}

impl MainBus {
    fn new() -> Self {
        Self {
            mem: vec![0x0; RAM_SIZE],
        }
    }
}

impl CommunicationInterface for MainBus {
    fn read(&self, address: Address) -> Option<Byte> {
        let addr = usize::from(address);
        if addr >= self.mem.len() {
            return None;
        }
        Some(self.mem[addr])
    }

    fn write(&mut self, address: Address, data: Byte) {
        let addr = usize::from(address);
        if addr >= self.mem.len() {
            return;
        }

        self.mem[addr] = data;
    }

    fn read_seq(&self, starting_address: Address, len: u16) -> Option<Vec<Byte>> {
        let mut result: Vec<Byte> = Vec::new();

        let limit: Address = starting_address + len;
        for address in starting_address..limit {
            if let Some(data) = self.read(address) {
                result.push(data);
            }
        } 

        if result.len() > 0 {
            return Some(result);
        }
        None
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

//
// Utility
//

const STACK_OFFSET: Address = 0x100;

impl Cpu {

    /// **stk_push()** - Pushes a byte to the stack stored in memory with offset `STACK_OFFSET`.
    /// **NB:** This routine will fail if no interface is connected.
    fn stk_push(&mut self, data: Byte) {
        let mut stk_ptr = self.regset().stk_ptr();
        let addr = STACK_OFFSET + Address::from(stk_ptr);
        self.writ_byte(addr, data);
        stk_ptr = stk_ptr.wrapping_sub(1);
        *self.regset_mut().stk_ptr_mut() = stk_ptr;
    }

    fn stk_doublepush(&mut self, data: Word) {
        self.stk_push((data >> 8) as u8);
        self.stk_push((data) as u8);
    }

    /// **stk_pop()** - Pops a byte from the stack stored in memory with offset `STACK_OFFSET`.
    /// **NB:** This routine will fail if no 1 passed; 0 failinterface is connected.
    fn stk_pop(&mut self) -> Byte {
        let mut stk_ptr = self.regset().stk_ptr();
        stk_ptr = stk_ptr.wrapping_add(1);
        let addr = STACK_OFFSET + Address::from(stk_ptr);
        let data = self.read_byte(addr);
        *self.regset_mut().stk_ptr_mut() = stk_ptr;
        data
    }
}

#[test]
fn test__stk_operations() {
    let mut cpu = Cpu::new_connected(Some(Rc::new(RefCell::new(MainBus::new()))));

    cpu.stk_push(0xcd);
    let cd = cpu.stk_pop();
    assert_eq!(cd, 0xcd);

    cpu.stk_push(0xab);
    let ab = cpu.stk_pop();
    assert_eq!(ab, 0xab);
}

#[test]
fn test__stk_operations_double() {
    let mut cpu = Cpu::new_connected(Some(Rc::new(RefCell::new(MainBus::new()))));
    let data: Word = 0xabcd;
    cpu.stk_doublepush(data);

    let cd = cpu.stk_pop();
    assert_eq!(cd, 0xcd);

    let ab = cpu.stk_pop();
    assert_eq!(ab, 0xab);

    let value = Word::from_le_bytes([cd, ab]);
    assert_eq!(value, 0xabcd);
}

#[test]
fn test__reset_cpu() {
    let mut cpu = Cpu::new_connected(Some(Rc::new(RefCell::new(MainBus::new()))));
    cpu.reset();

    let status_after_reset = cpu.regset().status();
    let unused = status_after_reset & 1 << 5 != 0;
    let overflowed = status_after_reset & 1 << 6 != 0;
    let carry = status_after_reset & 1 << 1 != 0;
    assert_eq!(unused, true);
    assert_eq!(overflowed, false);
    assert_eq!(carry, false);
}

#[test]
fn test__read_seq_from_host() {
    let cpu = Cpu::new_connected(Some(Rc::new(RefCell::new(MainBus::new()))));
    let result = cpu.read_some(0x000, 0x7ff);
    let expected_result = vec![0x00; 0x7ff];
    assert_eq!(expected_result, result);
}