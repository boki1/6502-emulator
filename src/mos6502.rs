use crate::mos6502::m6502_addressing_modes as other_m6502_addressing_modes;
use crate::mos6502::m6502_intruction_set::*;
use crate::mos6502::InterruptKind::Irq;
use getset::{CopyGetters, Getters, MutGetters, Setters};
use std::cell::RefCell;
use std::fmt::Display;
use std::fs::File;
use std::ops::Add;
use std::rc::Rc;
use std::{fmt, fmt::Debug};
use std::{io, io::prelude::*};

pub type Address = u16;
pub type Word = u16;
pub type Opcode = u8;
pub type Byte = u8;

pub type AddressingModeFn = fn(&mut Cpu) -> Result<AddressingOutput, CpuError>;
pub type InstructionFn = fn(&mut Cpu) -> Result<(), CpuError>;

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
///
/// impl RegisterSet {
///     #[inline]
///     fn carry(&self) -> bool {
///         (self.status & (1 << 7)) > 0
///     }
///
///     #[inline]
///     fn set_carry(&self, value: bool) {
///         self.status = (self.status & !(1 << 7)) | ((value as u8) << 7);
///     }
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
    accumulator: u8,
    x_index: u8,
    y_index: u8,
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

    fn new_custompc(custom_prog_counter: Address) -> Self {
        Self {
            prog_counter: custom_prog_counter,
            ..Self::new()
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum InterruptKind {
    Nmi,
    Irq,
}

/// **Jump vectors**
/// In any of this exception cases - NMI, IRQ, BRK or a RESET, the PC should jump to the
/// concrete address, also called _vector_.
/// There is no mistake in the values of `IRQ_VECTOR` and `BRK_VECTOR` - they
/// both point to 0xfffe.
const NMI_VECTOR: Address = 0xfffa;
const RESET_VECTOR: Address = 0xfffc;
const IRQ_VECTOR: Address = 0xfffe;
const BRK_VECTOR: Address = 0xfffe;

///
/// Cpu
///
/// The struct representation of the MOS 6502.
///
#[derive(Getters, CopyGetters, Setters, MutGetters)]
pub struct Cpu {
    /// **regset**
    /// The set of register that the cpu has
    #[getset(get_copy = "pub", get_mut = "pub")]
    regset: RegisterSet,

    /// **time**
    /// Information about the "timings" of
    /// this cpu instance. Stores information
    /// about the total elapsed clock cycles and
    /// also about the cycles remaining of the
    /// current instruction which is executing.
    #[getset(get_copy = "pub", get_mut = "pub")]
    time: Timings,

    /// **inter**
    /// Information about handling any interrupt
    /// "requests" which are pending to cpu
    /// instance.
    inter: InterruptHandling,

    /// **bus_conn**
    /// This cpu's connection to the "outer world".
    /// This field stores a mutable reference to a
    /// struct which has the trait CommunicationInterface,
    /// a.k.a can be read from and written to. This
    /// interface is stored as optional "shared pointer"
    /// using the interior mutability pattern.
    bus_conn: Option<Rc<RefCell<dyn CommunicationInterface>>>,

    /// **i**
    /// This field stores the current instruction which
    /// is being executed. It should be stored directly
    /// from the `cycle_clock()` function and not
    /// modified until `self.time.residual` is not
    /// decremented to 0. This is "residual time" is
    /// used in order to emulate clock cycle accuracy.
    /// Note the part with "emulate" and not "achieve".
    /// The current implementation is not clock cycle
    /// accurate.
    #[getset(get_mut = "pub")]
    i: Option<Instruction>,
}

///
/// CpuError
/// \
/// The enum describes the variation of errors which might occur during emulation.\
/// \
/// **BusInterfaceMissing** - The cpu has tried to read or write from its
/// associated memory but hasn't found one.\
/// **CurrentInstructionMissing** - The cpu has tried to access the current
/// intstruction field expecting to value correct value bot there has not
/// been one.\
/// **ExptectedOperandMissing** - An addressing mode which expects more than
/// the given operands has been used.\
/// **FailedLoadingProgram** - While reading the input file an error has occures.\
/// **BadAddressing** - This error occures either when addressing or when addressing
/// is exptected and it has not happened.
///
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CpuError {
    BusInterfaceMissing,
    CurrentInstructionMissing,
    ExpectedOperandMissing,
    FailedLoadingProgram,
    BadAddressing,
}

impl Cpu {
    /// Acts as post-fix increment operation (pc++):
    /// Increments the Program Counter and returns the **old** value.
    #[inline]
    fn inc_pc(&mut self) -> Word {
        let old_pc = self.regset.prog_counter;
        self.regset.prog_counter = old_pc.wrapping_add(1);
        old_pc
    }

    fn pc(&self) -> Word {
        self.regset.prog_counter
    }

    fn interrupt_handles(&self) -> &InterruptHandling {
        &self.inter
    }
}

impl std::fmt::Debug for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cpu")
            .field("regset", &self.regset)
            .field("time", &self.time)
            .field("inter", &self.inter)
            .field("is_attached", &self.bus_conn.is_some())
            .field("curr_i", &self.i)
            .finish()
    }
}

impl Cpu {
    /// **new()** - Creates a new instance of a cpu with its default
    /// field values
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
            i: None,
        }
    }

    /// **new_connected()** - Creates a new instance of a cpu with a bus interface
    /// supported
    pub(crate) fn new_connected(bus_conn: Option<Rc<RefCell<dyn CommunicationInterface>>>) -> Self {
        Self {
            bus_conn,
            ..Cpu::new()
        }
    }

    fn new_custompc(custom_prog_counter: Address) -> Self {
        Self {
            regset: RegisterSet::new_custompc(custom_prog_counter),
            ..Cpu::new()
        }
    }

    ///
    /// **clock_cycle()** - Perform a single cpu cycle
    ///
    /// This implementation is not cycle correct, but
    /// successfully emulates being so. This is done
    /// by keeping the amount of cycles which have to
    /// skipped/wasted after each actual instruction
    /// execution.
    fn clock_cycle(&mut self) {
        if self.time.residual() == 0 {
            let opcode = self.fetch();

            self.i = Some(Instruction::decode_by(opcode));
            *self.time_mut().residual_mut() = self.i.as_ref().unwrap().time;
            load_operand_curr_i(self);

            let address = self.i.as_ref().unwrap().amode_fun;
            if let Ok(amode_output) = address(self) {
                self.i.as_mut().unwrap().amode_output = amode_output;
            } else {
                panic!("Failed addressing");
            }

            let execute = self.i.as_ref().unwrap().fun;
            execute(self);
        }

        self.time_mut().next();
    }

    /// **inthandle()** - Handles any interrupts of the cpu.
    /// The different kinds of intterrupts which the MOS 6502 supports
    /// are BRK (software interrupt), IRQ (interrupt request) and
    /// NMI (non-maskable interrupt).
    /// NMIs cannot be disabled.
    /// In order to allow IRQs, the flag `irq_disabled` in the status
    /// register has to be clear.
    fn inthandle(&mut self, int: InterruptKind) -> bool {
        if int == Irq && self.regset().irq_disabled() {
            return false;
        }

        let prog_counter = self.regset().prog_counter();
        self.stk_doublepush(prog_counter);

        self.regset_mut().set_brk(false);
        self.regset_mut().set_unused(true);
        self.regset_mut().set_irq_disabled(true);

        let status = self.regset().status();
        self.stk_push(status);

        let (next_address, time) = match int {
            InterruptKind::Nmi => (0xFFFA, 8),
            InterruptKind::Irq => (0xFFFE, 7),
        };

        let new_pc = self.read_word(next_address);
        *self.regset_mut().set_prog_counter(new_pc);
        *self.time_mut().residual_mut() = time;
        true
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

        self.time = Timings {
            residual: 8,
            elapsed: 0,
        };
    }

    /// **connect()** - Connects the cpu to a bus, providing a context
    /// for read and write operations.
    fn connect_to(&mut self, conn: Rc<RefCell<dyn CommunicationInterface>>) {
        if let None = self.bus_conn {
            self.bus_conn = Some(conn);
        }
    }
}

impl Cpu {
    ///
    /// TODO FIXME:
    /// Consider returning Option<> or Result<> in order to
    /// give better return "code" to the called whether the
    /// value was actually 0 or an error occured. Same
    /// think goes for `writ_byte()` and the other wrapper
    /// functions.
    ///
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

    ///
    /// **writ_byte()** - Initiates a write request to the interface
    /// **if one is present**
    fn writ_byte(&self, address: Address, data: Byte) {
        if let Some(bus) = &self.bus_conn {
            return (*bus.borrow_mut()).write(address, data);
        }
    }

    ///
    /// **read_word()** - Wrapper function for reading two sequential
    /// bytes from the interface **if one is present**.
    fn read_word(&self, address: Address) -> Word {
        let lo = self.read_byte(address);
        let hi = self.read_byte(address + 1);
        Word::from_le_bytes([lo, hi])
    }

    ///
    /// **read_some()** - Reads sequence of bytes from the interface
    fn read_some(&self, address: Address, len: u16) -> Vec<Byte> {
        if let Some(bus) = &self.bus_conn {
            if let Some(result) = (*bus.borrow()).read_seq(address, len) {
                return result;
            }
        }

        vec![]
    }

    ///
    /// **fetch()** - Reads a byte from addressing the interface
    /// with the value of PC. After that the PC gets updated.
    #[inline]
    fn fetch(&mut self) -> Byte {
        let pc = self.inc_pc();
        self.read_byte(pc)
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

const RAM_SIZE: usize = 0xffff + 1;

/// The "host" of our cpu
/// Contains the contexual environment of the processor, most notably - memory.
pub struct MainBus {
    pub mem: Vec<Byte>,
}

impl MainBus {
    pub(crate) fn new() -> Self {
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

///
/// **make_instr!** - Makes a new Instruction instance with
/// given field values
///
#[macro_export]
macro_rules! make_instr {
    ($p_amode: ident, $p_fun: ident, $p_time: expr, $p_mnemonic: literal, $p_size: expr) => {
        Instruction {
            amode: $p_amode,
            amode_fun: m6502_addressing_modes::to_fun($p_amode),
            fun: $p_fun,
            time: $p_time,
            mnemonic: String::from($p_mnemonic),
            size: $p_size,
            amode_output: AddressingOutput::NotExecuted,
            operand: None,
            loaded_from: 0x0000,
        }
    };
}

///
/// **make_illegal!** - Makes a new illegal instruction instance
/// with given field values
///
macro_rules! make_illegal {
    () => {
        unimplemented!();
    };
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AddressingOutput {
    Fetched { value: Byte, address: Address },
    ValueOnly(Byte),
    AbsoluteAddress(Word),
    NotExecuted,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AddressingMode {
    Imp,
    Imm,
    Zp0,
    Zpx,
    Zpy,
    Abs,
    Abx,
    Aby,
    Ind,
    Iny,
    Inx,
    Rel,
}

impl Display for AddressingMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

///
/// Instruction
///
/// This structure represents a single operatation in
/// the 6502 cpu. It contains meta information about
/// the instruction such as its addressing mode and
/// its time to execute and some information about
/// its surroundings. The context contains information
/// such as the operand used and its final result
/// which might be a value (Fetched) or a address
/// (Absolute Address).
pub struct Instruction {
    ///
    /// Meta information
    amode_fun: AddressingModeFn,
    amode: AddressingMode,
    fun: InstructionFn,
    time: u8,
    mnemonic: String,
    size: u16,

    /// Instruction context
    ///
    /// **operand** contains the value of the
    /// operands this instruction is using. They
    /// might be either zero or one element.\
    /// \
    /// It is modified **before** the function
    /// `amode` and used in it in order to produce
    /// `amode_output`.
    operand: Option<Word>,

    /// **amode_output** - The "output" of
    /// the process of addressing this instruction.
    /// It might be either an address or a value
    /// fetched from memory. \
    /// \
    /// It is modified **only from the function `amode`.
    amode_output: AddressingOutput,

    /// **loaded_from** - The address where the first byte of this
    /// instruction is located in memory
    ///
    /// Unused in the current implementation.
    loaded_from: Address,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        use AddressingMode::*;

        let details = match self.amode {
            Imm => ("#", ""),
            Imp | Zp0 | Abs | Rel => ("", ""),
            Ind => ("(", ")"),
            Abx | Zpx => ("", ", X"),
            Aby | Zpy => ("", ", Y"),
            Iny => ("(", "), Y"),
            Inx => ("(", ", X)"),
        };

        let address = format!("{:#6x?}\t", self.loaded_from);
        let operand = match self.operand {
            Some(num) => format!("{:#4x?}", num),
            None => String::new(),
        };
        let addressing_mode = format!("\t| {}", self.amode);

        write!(
            f,
            "{}",
            format!(
                "{address}{mnemonic}\t{prefix}{operand}{suffix}{addressing_mode}\n",
                address = address,
                mnemonic = self.mnemonic,
                prefix = details.0,
                operand = operand,
                suffix = details.1,
                addressing_mode = addressing_mode
            )
        )
    }
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.debug_struct("Instruction")
            .field("time", &self.time)
            .field("mnemonic", &self.mnemonic)
            .field("size", &self.size)
            .finish()
    }
}

impl PartialEq for Instruction {
    fn eq(&self, other: &Self) -> bool {
        self.time == other.time
            && self.mnemonic == other.mnemonic
            && self.size == other.size
            && self.fun as usize == other.fun as usize
            && self.amode_fun as usize == other.amode_fun as usize
    }
}

impl Clone for Instruction {
    fn clone(&self) -> Self {
        Self {
            amode_fun: self.amode_fun,
            amode: self.amode,
            fun: self.fun,
            time: self.time,
            mnemonic: self.mnemonic.clone(),
            size: self.size,
            operand: self.operand.clone(),
            amode_output: self.amode_output,
            loaded_from: self.loaded_from,
        }
    }
}

type Operand = Option<Word>;
pub type InstructionMeta = (Operand, AddressingOutput);

impl Instruction {
    ///
    /// **decode_by** - Match a given opcode value to its corresponding
    /// instruction.
    ///
    /// The table is filled according to [this](https://www.masswerk.at/6502/6502_instruction_set.html) resource.
    ///
    /// **NB:** Illegal opcodes are not supported as of now
    ///
    fn decode_by(opcode: Byte) -> Instruction {
        use m6502_addressing_modes::*;
        use m6502_intruction_set::*;
        use AddressingMode::*;

        return match opcode {
            // opcode => make_instr! (
            //              addr_mode,
            //              instruction,
            //              cycles,
            //              instruction_name,
            //              size
            //          )
            0x00 => make_instr!(Imp, brk, 7, "brk", 1),
            0x01 => make_instr!(Inx, ora, 6, "ora", 2),
            0x05 => make_instr!(Zp0, ora, 3, "ora", 2),
            0x06 => make_instr!(Zp0, asl, 5, "asl", 2),
            0x08 => make_instr!(Imp, php, 3, "php", 1),
            0x09 => make_instr!(Imm, ora, 2, "ora", 2),
            0x0A => make_instr!(Imp, asl, 2, "asl", 1),
            0x0D => make_instr!(Abs, ora, 4, "ora", 3),
            0x0E => make_instr!(Abs, asl, 6, "asl", 3),

            0x10 => make_instr!(Rel, bpl, 2 /* or 3 */, "bpl", 2),
            0x11 => make_instr!(Iny, ora, 5, "ora", 2),
            0x15 => make_instr!(Zpx, ora, 4, "ora", 2),
            0x16 => make_instr!(Zpx, asl, 6, "asl", 2),
            0x18 => make_instr!(Imp, clc, 2, "clc", 1),
            0x19 => make_instr!(Aby, ora, 4, "ora", 3),
            0x1D => make_instr!(Abx, ora, 4, "ora", 3),
            0x1E => make_instr!(Abx, asl, 7, "asl", 3),

            0x20 => make_instr!(Abs, jsr, 6, "jsr", 3),
            0x21 => make_instr!(Inx, and, 6, "and", 2),
            0x24 => make_instr!(Zp0, bit, 3, "bit", 2),
            0x25 => make_instr!(Zp0, and, 3, "and", 2),
            0x26 => make_instr!(Zp0, rol, 5, "rol", 2),
            0x28 => make_instr!(Imp, plp, 4, "plp", 1),
            0x29 => make_instr!(Imm, and, 2, "and", 2),
            0x2A => make_instr!(Imp, rol, 2, "rol", 1),
            0x2C => make_instr!(Abs, bit, 4, "bit", 3),
            0x2D => make_instr!(Abs, and, 4, "and", 3),
            0x2E => make_instr!(Abs, rol, 6, "rol", 3),

            0x30 => make_instr!(Rel, bmi, 2 /* or 3 */, "bmi", 2),
            0x31 => make_instr!(Iny, and, 5, "and", 2),
            0x35 => make_instr!(Zpx, and, 4, "and", 2),
            0x36 => make_instr!(Zpx, rol, 6, "rol", 2),
            0x38 => make_instr!(Imp, sec, 2, "sec", 1),
            0x39 => make_instr!(Aby, and, 4, "and", 3),
            0x3D => make_instr!(Abx, and, 4, "and", 3),
            0x3E => make_instr!(Abx, rol, 7, "rol", 3),

            0x40 => make_instr!(Imp, rti, 6, "rti", 1),
            0x41 => make_instr!(Inx, eor, 6, "eor", 2),
            0x45 => make_instr!(Zp0, eor, 3, "eor", 2),
            0x46 => make_instr!(Zp0, lsr, 5, "lsr", 2),
            0x48 => make_instr!(Imp, pha, 3, "pha", 1),
            0x49 => make_instr!(Imm, eor, 2, "eor", 2),
            0x4A => make_instr!(Imp, lsr, 2, "lsr", 1),
            0x4C => make_instr!(Abs, jmp, 3, "jmp", 3),
            0x4D => make_instr!(Abs, eor, 4, "eor", 3),
            0x4E => make_instr!(Abs, lsr, 6, "lsr", 3),

            0x50 => make_instr!(Rel, bvc, 2 /* or 3 */, "bvc", 2),
            0x51 => make_instr!(Iny, eor, 5, "eor", 2),
            0x55 => make_instr!(Zpx, eor, 4, "eor", 2),
            0x56 => make_instr!(Zpx, lsr, 6, "lsr", 2),
            0x58 => make_instr!(Imp, cli, 2, "cli", 1),
            0x59 => make_instr!(Aby, eor, 4, "eor", 3),
            0x5D => make_instr!(Abx, eor, 4, "eor", 3),
            0x5E => make_instr!(Abx, lsr, 7, "lsr", 3),

            0x60 => make_instr!(Imp, rts, 6, "rts", 1),
            0x61 => make_instr!(Inx, adc, 6, "adc", 2),
            0x65 => make_instr!(Zp0, adc, 3, "adc", 2),
            0x66 => make_instr!(Zp0, ror, 5, "ror", 2),
            0x68 => make_instr!(Imp, pla, 4, "pla", 1),
            0x69 => make_instr!(Imm, adc, 2, "adc", 2),
            0x6A => make_instr!(Imp, ror, 2, "ror", 1),
            0x6C => make_instr!(Ind, jmp, 5, "jmp", 3),
            0x6D => make_instr!(Abs, adc, 4, "adc", 3),
            0x6E => make_instr!(Abx, ror, 7, "ror", 3),

            0x70 => make_instr!(Rel, bvs, 2 /* or 3 */, "bvs", 2),
            0x71 => make_instr!(Iny, adc, 5, "adc", 2),
            0x75 => make_instr!(Zpx, adc, 4, "adc", 2),
            0x76 => make_instr!(Zpx, ror, 6, "ror", 2),
            0x78 => make_instr!(Imp, sei, 2, "sei", 1),
            0x79 => make_instr!(Aby, adc, 4, "adc", 3),
            0x7D => make_instr!(Abx, adc, 4, "adc", 3),
            0x7E => make_instr!(Abs, ror, 6, "ror", 6),

            0x81 => make_instr!(Inx, sta, 6, "sta", 2),
            0x84 => make_instr!(Zp0, sty, 3, "sty", 2),
            0x85 => make_instr!(Zp0, sta, 3, "sta", 2),
            0x86 => make_instr!(Zp0, stx, 3, "stx", 2),
            0x88 => make_instr!(Imp, dey, 2, "dey", 1),
            0x8A => make_instr!(Imp, txa, 2, "txa", 1),
            0x8C => make_instr!(Abs, sty, 4, "sty", 3),
            0x8D => make_instr!(Abs, sta, 4, "sta", 3),
            0x8E => make_instr!(Abs, stx, 4, "stx", 3),

            0x90 => make_instr!(Rel, bcc, 2 /* or 3 */, "bcc", 2),
            0x91 => make_instr!(Iny, sta, 6, "sta", 2),
            0x94 => make_instr!(Zpx, sty, 4, "sty", 2),
            0x95 => make_instr!(Zpx, sta, 4, "sta", 2),
            0x96 => make_instr!(Zpy, stx, 4, "stx", 2),
            0x98 => make_instr!(Imp, tya, 2, "tya", 1),
            0x99 => make_instr!(Aby, sta, 5, "sta", 3),
            0x9A => make_instr!(Imp, txs, 2, "txs", 1),
            0x9D => make_instr!(Abx, sta, 5, "sta", 3),

            0xA0 => make_instr!(Imm, ldy, 2, "ldy", 2),
            0xA1 => make_instr!(Inx, lda, 6, "lda", 2),
            0xA2 => make_instr!(Imm, ldx, 2, "ldx", 2),
            0xA4 => make_instr!(Zp0, ldy, 3, "ldy", 2),
            0xA5 => make_instr!(Zp0, lda, 3, "lda", 2),
            0xA6 => make_instr!(Zp0, ldx, 3, "lda", 2),
            0xA8 => make_instr!(Imp, tay, 2, "tay", 1),
            0xA9 => make_instr!(Imm, lda, 2, "lda", 2),
            0xAA => make_instr!(Imp, tax, 2, "tax", 1),
            0xAC => make_instr!(Abs, ldy, 4, "ldy", 3),
            0xAD => make_instr!(Abs, lda, 4, "lda", 3),
            0xAE => make_instr!(Abs, ldx, 4, "ldx", 3),

            0xB0 => make_instr!(Rel, bcs, 2 /* or 3 */, "bcs", 2),
            0xB1 => make_instr!(Iny, lda, 5, "lda", 2),
            0xB4 => make_instr!(Zpx, ldy, 4, "ldy", 2),
            0xB5 => make_instr!(Zpx, lda, 4, "lda", 2),
            0xB6 => make_instr!(Zpy, ldx, 4, "ldx", 2),
            0xB8 => make_instr!(Imp, clv, 2, "clv", 1),
            0xB9 => make_instr!(Aby, lda, 4, "lda", 3),
            0xBA => make_instr!(Imp, tsx, 2, "tsx", 1),
            0xBC => make_instr!(Abx, ldy, 4, "ldy", 3),
            0xBD => make_instr!(Abx, lda, 4, "lda", 3),
            0xBE => make_instr!(Aby, ldx, 4, "ldx", 3),

            0xC0 => make_instr!(Imm, cpy, 2, "cpy", 2),
            0xC1 => make_instr!(Inx, cmp, 6, "cmp", 2),
            0xC4 => make_instr!(Zp0, cpy, 3, "cpy", 2),
            0xC5 => make_instr!(Zp0, cmp, 3, "cmp", 2),
            0xC6 => make_instr!(Zp0, dec, 5, "dec", 2),
            0xC8 => make_instr!(Imp, iny, 2, "iny", 1),
            0xC9 => make_instr!(Imm, cmp, 2, "cmp", 2),
            0xCA => make_instr!(Imp, dex, 2, "dex", 1),
            0xCC => make_instr!(Abs, cpy, 4, "cpy", 3),
            0xCD => make_instr!(Abs, cmp, 4, "cmp", 3),
            0xCE => make_instr!(Abs, dec, 6, "dec", 3),

            0xD0 => make_instr!(Rel, bne, 2 /* or 3 */, "bne", 2),
            0xD1 => make_instr!(Iny, cmp, 5, "cmp", 2),
            0xD5 => make_instr!(Zpx, cmp, 4, "cmp", 2),
            0xD6 => make_instr!(Zpx, dec, 6, "dec", 2),
            0xD8 => make_instr!(Imp, cld, 2, "cld", 1),
            0xD9 => make_instr!(Aby, cmp, 4, "cmp", 3),
            0xDD => make_instr!(Abx, cmp, 4, "cmp", 3),
            0xDE => make_instr!(Abx, dec, 7, "dec", 3),

            0xE0 => make_instr!(Imm, cpx, 2, "cpx", 2),
            0xE1 => make_instr!(Inx, sbc, 6, "sbc", 2),
            0xE4 => make_instr!(Zp0, cpx, 3, "cpx", 2),
            0xE5 => make_instr!(Zp0, sbc, 3, "sbc", 2),
            0xE6 => make_instr!(Zp0, inc, 5, "inc", 2),
            0xE8 => make_instr!(Imp, inx, 2, "inx", 1),
            0xE9 => make_instr!(Imm, sbc, 2, "sbc", 2),
            0xEA => make_instr!(Imp, nop, 2, "nop", 1),
            0xEC => make_instr!(Abs, cpx, 4, "cpx", 3),
            0xED => make_instr!(Abs, sbc, 4, "sbc", 3),
            0xEE => make_instr!(Abs, inc, 6, "inc", 3),

            0xF0 => make_instr!(Rel, beq, 2 /* or 3 */, "beq", 2),
            0xF1 => make_instr!(Iny, sbc, 5, "sbc", 2),
            0xF5 => make_instr!(Zpx, sbc, 4, "sbc", 2),
            0xF6 => make_instr!(Zpx, inc, 6, "inc", 2),
            0xF8 => make_instr!(Imp, sed, 2, "sed", 1),
            0xF9 => make_instr!(Aby, sbc, 4, "sbc", 3),
            0xFD => make_instr!(Abx, sbc, 4, "sbc", 3),
            0xFE => make_instr!(Abx, inc, 7, "inc", 3),

            _ => make_illegal!(),
        };
    }

    fn meta(&self) -> InstructionMeta {
        (self.operand, self.amode_output)
    }

    fn set_meta(&mut self, meta: InstructionMeta) {
        self.operand = meta.0;
        self.amode_output = meta.1;
    }
}

///
/// Instruction set
/// Legal MOS 6502 instructions
///
mod m6502_intruction_set {
    use crate::mos6502::Byte;
use crate::mos6502::RegisterSet;
use super::{
        Address, AddressingMode::*, AddressingOutput::*, Cpu, CpuError, Instruction, MainBus,
        Opcode, Word,
    };
    use crate::mos6502::BRK_VECTOR;

    fn verify_and_fetch(cpu: &mut Cpu) -> Result<Word, CpuError> {
        if let Some(i) = &cpu.i {
            return match &i.amode_output {
                Fetched { value, address } => Ok(Word::from(*value)),
                ValueOnly(value) => Ok(Word::from(*value)),
                AbsoluteAddress(address) => Ok(*address),
                NotExecuted => Err(CpuError::BadAddressing),
            };
        }

        Err(CpuError::CurrentInstructionMissing)
    }

    #[inline]
    fn page_of(addr: Address) -> u8 {
        ((addr & 0xff00) >> 8) as u8
    }

    /// Performs the actual branch. Gets called by all 'Branch if ...'
    /// instructions if their specific predicate is correct (??? is that the right word)
    /// According to (this )[] website the following rules apply to all branching
    /// instructions.
    /// > Add 1 to cycles if branch occurs on same page.
    /// > Add 2 to cycles if branch occurs to different page.
    fn do_branch(cpu: &mut Cpu, next_address: Address) {
        let prog_counter = cpu.regset().prog_counter();
        let cycles_inc = if page_of(next_address) == page_of(prog_counter) {
            1
        } else {
            2
        };

        *cpu.time_mut().residual_mut() += cycles_inc;
        cpu.regset_mut().set_prog_counter(next_address);
    }

    fn do_compare(regs: &mut RegisterSet, lhs: Byte, rhs: Byte) {
        let diff = lhs.wrapping_sub(rhs);

        regs.set_carry(lhs >= rhs);
        regs.set_zero(lhs == rhs);
        regs.set_negative(diff & 0x80 > 0);
    }

    /// TODO:
    /// Document

    /// adc - Add with carry
    ///
    /// V <- ~(A^M) & A^(A+M+C)
    /// V = ~(A^M) & (A^R)
    pub fn adc(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched = verify_and_fetch(cpu)?;

        let regs = cpu.regset_mut();
        let accumulator = u16::from(regs.accumulator());
        let tmp: u16 = accumulator + fetched + u16::from(regs.carry());

        regs.set_carry(tmp > 0xFF);
        regs.set_zero((tmp & 0x00FF) == 0);
        regs.set_negative((tmp & 0x80) > 0);
        regs.set_overflowed((!(accumulator ^ fetched) & (accumulator ^ tmp) & 0x80) > 0);

        regs.set_accumulator((tmp & 0x00FF) as u8);

        Ok(())
    }

    pub fn and(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched_u16 = verify_and_fetch(cpu)?;
        let fetched = fetched_u16 as u8;

        let a = cpu.regset().accumulator & fetched;
        let regs = cpu.regset_mut();

        regs.set_accumulator(a);

        regs.set_zero(a == 0x00);
        regs.set_negative(a & 0x80 > 0);

        Ok(())
    }

    pub fn asl(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched_u16 = verify_and_fetch(cpu)?;
        let fetched = fetched_u16 as u8;

        let tmp = fetched_u16 << 1;
        let lo = (tmp & 0x00FF) as u8;

        if let Some(i) = cpu.i.as_ref() {
            match i.amode {
                Imp => {
                    let _ = cpu.regset_mut().set_accumulator(lo);
                }
                _ => {
                    if let Fetched { value: _, address } = i.amode_output {
                        cpu.writ_byte(address, lo);
                    } else {
                        panic!("Bad addressing mode usage. Expected address as well as value.")
                    }
                }
            };
        }

        let regs = cpu.regset_mut();
        regs.set_carry((tmp & 0xFF00) > 0);
        regs.set_zero(lo == 0);
        regs.set_negative((tmp & 0x80) > 0);

        Ok(())
    }

    pub fn bcc(cpu: &mut Cpu) -> Result<(), CpuError> {
        let next_address = verify_and_fetch(cpu)?;
        let regs = cpu.regset();

        if !regs.carry() {
            do_branch(cpu, next_address);
        }

        Ok(())
    }

    pub fn bcs(cpu: &mut Cpu) -> Result<(), CpuError> {
        let next_address = verify_and_fetch(cpu)?;
        let regs = cpu.regset();

        if regs.carry() {
            do_branch(cpu, next_address);
        }

        Ok(())
    }

    pub fn beq(cpu: &mut Cpu) -> Result<(), CpuError> {
        let next_address = verify_and_fetch(cpu)?;
        let regs = cpu.regset();

        if regs.zero() {
            do_branch(cpu, next_address);
        }

        Ok(())
    }

    pub fn bit(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched = verify_and_fetch(cpu)?;

        let regs = cpu.regset_mut();
        let a = regs.accumulator();
        regs.set_zero(u16::from(a) & fetched == 0);
        regs.set_negative(fetched & (1 << 7) > 0);
        regs.set_overflowed(fetched & (1 << 6) > 0);

        Ok(())
    }

    pub fn bmi(cpu: &mut Cpu) -> Result<(), CpuError> {
        let next_address = verify_and_fetch(cpu)?;
        let regs = cpu.regset();

        if regs.negative() {
            do_branch(cpu, next_address);
        }

        Ok(())
    }

    pub fn bne(cpu: &mut Cpu) -> Result<(), CpuError> {
        let next_address = verify_and_fetch(cpu)?;
        let regs = cpu.regset();

        if !regs.zero() {
            do_branch(cpu, next_address);
        }

        Ok(())
    }

    pub fn bpl(cpu: &mut Cpu) -> Result<(), CpuError> {
        let next_address = verify_and_fetch(cpu)?;
        let regs = cpu.regset();

        if !regs.negative() {
            do_branch(cpu, next_address);
        }

        Ok(())
    }

    pub fn brk(cpu: &mut Cpu) -> Result<(), CpuError> {
        cpu.inc_pc();
        cpu.regset_mut().set_irq_disabled(true);

        cpu.stk_doublepush(cpu.pc());

        cpu.regset_mut().set_brk(true);
        let status = cpu.regset().status();
        cpu.stk_push(status);
        cpu.regset_mut().set_brk(false);

        let new_pc = cpu.read_word(BRK_VECTOR);
        cpu.regset_mut().set_prog_counter(new_pc);

        Ok(())
    }

    pub fn bvc(cpu: &mut Cpu) -> Result<(), CpuError> {
        let next_address = verify_and_fetch(cpu)?;
        let regs = cpu.regset();

        if !regs.overflowed() {
            do_branch(cpu, next_address);
        }

        Ok(())
    }

    pub fn bvs(cpu: &mut Cpu) -> Result<(), CpuError> {
        let next_address = verify_and_fetch(cpu)?;
        let regs = cpu.regset();

        if regs.overflowed() {
            do_branch(cpu, next_address);
        }

        Ok(())
    }

    pub fn clc(cpu: &mut Cpu) -> Result<(), CpuError> {
        cpu.regset_mut().set_carry(false);
        Ok(())
    }

    pub fn cld(cpu: &mut Cpu) -> Result<(), CpuError> {
        cpu.regset_mut().set_decimal_mode(false);
        Ok(())
    }

    pub fn cli(cpu: &mut Cpu) -> Result<(), CpuError> {
        cpu.regset_mut().set_irq_disabled(false);

        Ok(())
    }

    pub fn clv(cpu: &mut Cpu) -> Result<(), CpuError> {
        cpu.regset_mut().set_overflowed(false);

        Ok(())
    }

    pub fn cmp(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched = verify_and_fetch(cpu)? as u8;
        let regs: &mut RegisterSet = cpu.regset_mut();

        do_compare(regs, regs.accumulator(), fetched);

        Ok(())
    }

    pub fn cpx(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched = verify_and_fetch(cpu)? as u8;
        let regs: &mut RegisterSet = cpu.regset_mut();

        do_compare(regs, regs.x_index(), fetched);

        Ok(())
    }

    pub fn cpy(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched = verify_and_fetch(cpu)? as u8;
        let regs = cpu.regset_mut();

        do_compare(regs, regs.y_index(), fetched);

        Ok(())
    }

    pub fn dec(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn dex(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn dey(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn eor(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn inc(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched = (verify_and_fetch(cpu)? as u8).wrapping_add(1);
        let amode_output = cpu.i.as_ref().unwrap().amode_output;

        let address = match amode_output {
            Fetched { value: _, address } => address,
            AbsoluteAddress(address) => address,
            _ => return Err(CpuError::BadAddressing)
        };

        cpu.writ_byte(address, fetched);
        let regs = cpu.regset_mut();
        regs.set_zero(fetched == 0);
        regs.set_negative(fetched & 0x80 > 0);

        Ok(())
    }

    pub fn inx(cpu: &mut Cpu) -> Result<(), CpuError> {
        let x = cpu.regset().x_index().wrapping_add(1);
        let regs = cpu.regset_mut();

        regs.set_zero(x == 0);
        regs.set_negative(x & 0x80 > 0);

        regs.set_x_index(x);

        Ok(())
    }

    pub fn iny(cpu: &mut Cpu) -> Result<(), CpuError> {
        let y = cpu.regset().y_index().wrapping_add(1);
        let regs = cpu.regset_mut();

        regs.set_zero(y == 0);
        regs.set_negative(y & 0x80 > 0);

        regs.set_y_index(y);

        Ok(())
    }

    pub fn jmp(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn jsr(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn lda(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched = verify_and_fetch(cpu)? as u8;

        let regs = cpu.regset_mut();
        regs.set_accumulator(fetched);

        regs.set_zero(fetched == 0);
        regs.set_negative(fetched & 0x80 > 0);

        Ok(())
    }

    pub fn ldx(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched = verify_and_fetch(cpu)? as u8;

        let regs = cpu.regset_mut();
        regs.set_x_index(fetched);

        regs.set_zero(fetched == 0);
        regs.set_negative(fetched & 0x80 > 0);

        Ok(())
    }

    pub fn ldy(cpu: &mut Cpu) -> Result<(), CpuError> {
        let fetched = verify_and_fetch(cpu)? as u8;

        let regs = cpu.regset_mut();
        regs.set_y_index(fetched);

        regs.set_zero(fetched == 0);
        regs.set_negative(fetched & 0x80 > 0);

        Ok(())
    }

    pub fn lsr(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn nop(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn ora(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn pha(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn php(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn pla(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn plp(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn rol(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn ror(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn rti(cpu: &mut Cpu) -> Result<(), CpuError> {
        let loaded_status = cpu.stk_pop();
        let lo = cpu.stk_pop();
        let hi = cpu.stk_pop();
        let loaded_pc = Address::from_le_bytes([lo, hi]);

        let regs = cpu.regset_mut();
        regs.set_prog_counter(loaded_pc);
        regs.set_status(loaded_status);

        regs.set_brk(false);
        regs.set_unused(false);

        Ok(())
    }

    pub fn rts(cpu: &mut Cpu) -> Result<(), CpuError> {
        let lo = cpu.stk_pop();
        let hi = cpu.stk_pop();
        let loaded_pc = Address::from_le_bytes([lo, hi]);

        cpu.regset_mut().set_prog_counter(loaded_pc + 1);

        Ok(())
    }

    pub fn sbc(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn sec(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn sed(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn sei(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn sta(cpu: &mut Cpu) -> Result<(), CpuError> {
        let _ = verify_and_fetch(cpu)?;

        let a = cpu.regset().accumulator();

        if let Some(i) = cpu.i.as_ref() {
            if let Fetched {value: _, address} = i.amode_output {
                cpu.writ_byte(address, a);
                return Ok(());
            } else {
                return Err(CpuError::BadAddressing);
            }
        }

        Err(CpuError::CurrentInstructionMissing)
    }

    pub fn stx(cpu: &mut Cpu) -> Result<(), CpuError> {
        let _ = verify_and_fetch(cpu)?;

        let x = cpu.regset().x_index();

        if let Some(i) = cpu.i.as_ref() {
            if let Fetched {value: _, address} = i.amode_output {
                cpu.writ_byte(address, x);
                return Ok(());
            } else {
                return Err(CpuError::BadAddressing);
            }
        }

        Err(CpuError::CurrentInstructionMissing)
    }

    pub fn sty(cpu: &mut Cpu) -> Result<(), CpuError> {
        let _ = verify_and_fetch(cpu)?;

        let y = cpu.regset().y_index();

        if let Some(i) = cpu.i.as_ref() {
            if let Fetched {value: _, address} = i.amode_output {
                cpu.writ_byte(address, y);
                return Ok(());
            } else {
                return Err(CpuError::BadAddressing);
            }
        }

        Err(CpuError::CurrentInstructionMissing)
    }

    pub fn tax(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn tay(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn tsx(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn txa(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn txs(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    pub fn tya(cpu: &mut Cpu) -> Result<(), CpuError> {
        Ok(())
    }

    #[cfg(test)]
    mod test {

        use crate::mos6502::AddressingOutput;
use super::*;
        use std::cell::RefCell;
        use std::rc::Rc;

        fn setup(
            custom_pc: Word,
            connect: bool,
            opcode: Option<Opcode>,
            operand: Option<Word>,
        ) -> Cpu {
            let mut cpu = Cpu::new_custompc(custom_pc);

            if let Some(opc) = opcode {
                cpu.i = Some(Instruction::decode_by(opc));
                cpu.i.as_mut().unwrap().operand = operand;
            }

            if connect {
                cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
            }

            cpu
        }

        fn set_amode_output(cpu: &mut Cpu, amode_output: AddressingOutput) {
            cpu.i.as_mut().unwrap().amode_output = amode_output;
        }

        #[test]
        fn test_adc_regular() {
            let mut cpu = setup(0x0000, true, Some(0x69), Some(0x10));
            set_amode_output(&mut cpu, ValueOnly(0x10));

            let res = adc(&mut cpu);

            assert_eq!(res.ok(), Some(()));
            let regs = cpu.regset();
            assert_eq!(regs.accumulator(), 0x10);
            assert_eq!(regs.carry(), false);
            assert_eq!(regs.zero(), false);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.overflowed(), false);
        }

        #[test]
        fn test_adc_negative() {
            let mut cpu = setup(0x0000, true, Some(0x69), Some(0x80));
            set_amode_output(&mut cpu, ValueOnly(0x80));

            let res = adc(&mut cpu);

            assert_eq!(res.ok(), Some(()));
            let regs = cpu.regset();
            assert_eq!(regs.accumulator(), 0x80);
            assert_eq!(regs.carry(), false);
            assert_eq!(regs.zero(), false);
            assert_eq!(regs.negative(), true);
            assert_eq!(regs.overflowed(), false);
        }

        #[test]
        fn test_adc_zero() {
            let mut cpu = setup(0x0000, true, Some(0x69), Some(0x00));
            set_amode_output(&mut cpu, ValueOnly(0x00));

            let res = adc(&mut cpu);

            assert_eq!(res.ok(), Some(()));
            let regs = cpu.regset();
            assert_eq!(regs.accumulator(), 0x0);
            assert_eq!(regs.carry(), false);
            assert_eq!(regs.zero(), true);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.overflowed(), false);
        }

        #[test]
        fn test_adc_carry() {
            let mut cpu = setup(0x0000, true, Some(0x69), Some(0x01));
            set_amode_output(&mut cpu, ValueOnly(0x01));
            cpu.regset_mut().set_accumulator(0xFF);

            let res = adc(&mut cpu);

            assert_eq!(res.ok(), Some(()));
            let regs = cpu.regset();
            assert_eq!(regs.accumulator(), 0x00);
            assert_eq!(regs.carry(), true);
            assert_eq!(regs.zero(), true);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.overflowed(), false);
        }

        #[test]
        fn test_adc_overflowed() {
            let mut cpu = setup(0x0000, true, Some(0x69), Some(0x01));
            set_amode_output(&mut cpu, ValueOnly(0x01));
            cpu.regset_mut().set_accumulator(0x7F);

            let res = adc(&mut cpu);

            assert_eq!(res.ok(), Some(()));
            let regs = cpu.regset();
            assert_eq!(regs.accumulator(), 0x80);
            assert_eq!(regs.carry(), false);
            assert_eq!(regs.zero(), false);
            assert_eq!(regs.negative(), true);
            assert_eq!(regs.overflowed(), true);
        }

        #[test]
        fn test_and_zero() {
            let mut cpu = setup(0x0000, true, Some(0x29), Some(0x0));
            set_amode_output(&mut cpu, ValueOnly(0x00));
            cpu.regset_mut().set_accumulator(0x07);

            let res = and(&mut cpu);

            assert_eq!(res.ok(), Some(()));
            let regs = cpu.regset();
            assert_eq!(regs.accumulator(), 0x00);
            assert_eq!(regs.zero(), true);
            assert_eq!(regs.negative(), false);
        }

        #[test]
        fn test_and_negative() {
            let mut cpu = setup(0x0000, true, Some(0x29), Some(0x80));
            set_amode_output(&mut cpu, ValueOnly(0x80));
            cpu.regset_mut().set_accumulator(0x95);

            let res = and(&mut cpu);

            assert_eq!(res.ok(), Some(()));
            let regs = cpu.regset();
            assert_eq!(regs.accumulator(), 0x80);
            assert_eq!(regs.zero(), false);
            assert_eq!(regs.negative(), true);
        }

        #[test]
        fn test_asl_mem() {
            let mut cpu = setup(0x0000, true, Some(0x06), Some(0xFF));
            set_amode_output(&mut cpu, Fetched { value: 0x4F, address: 0xFF});
            cpu.regset_mut().set_accumulator(0x00);

            let res = asl(&mut cpu);
            let shifted = cpu.read_byte(0xFF);

            assert_eq!(res.ok(), Some(()));
            assert_eq!(shifted, 0x4F << 1);
            let regs = cpu.regset();
            assert_eq!(regs.negative(), true);
            assert_eq!(regs.zero(), false);
            assert_eq!(regs.carry(), false);
        }

        #[test]
        fn test_asl_accumulator() {
            let mut cpu = setup(0x0000, true, Some(0x0A), None);
            cpu.regset_mut().set_accumulator(0x4);
            set_amode_output(&mut cpu, ValueOnly(0x4));

            let res = asl(&mut cpu);
            let regs = cpu.regset();

            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.accumulator(), 0x8);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.zero(), false);
            assert_eq!(regs.carry(), false);
        }

        #[test]
        fn test_do_branch() {
            let mut cpu = setup(0xFEBE, true, None, None);
            *cpu.time_mut().residual_mut() = 0;

            do_branch(&mut cpu, 0xFEBE + 10);
            let regs = cpu.regset();

            assert_eq!(cpu.pc(), 0xFEBE + 10);
            assert_eq!(cpu.time.residual(), 1);
        }

        #[test]
        fn test_do_branch_negative() {
            let mut cpu = setup(0xFEBE, true, None, None);
            *cpu.time_mut().residual_mut() = 0;

            do_branch(&mut cpu, 0xFEBE - 120);
            let regs = cpu.regset();

            assert_eq!(cpu.pc(), 0xFEBE - 120);
            assert_eq!(cpu.time.residual(), 1);
        }

        #[test]
        fn test_do_branch_page_cross() {
            let mut cpu = setup(0xFEFE, true, None, None);
            *cpu.time_mut().residual_mut() = 0;

            do_branch(&mut cpu, 0xFEFE + 10);

            assert_eq!(cpu.pc(), 0xFEFE + 10);
            assert_eq!(cpu.time.residual(), 2);
        }

        #[test]
        fn test_do_branch_negative_page_cross() {
            let mut cpu = setup(0xFEFE + 10, true, None, None);
            *cpu.time_mut().residual_mut() = 0;

            do_branch(&mut cpu, 0xFEFE);

            assert_eq!(cpu.pc(), 0xFEFE);
            assert_eq!(cpu.time.residual(), 2);
        }

        #[test]
        fn test_bit() {
            let mut cpu = setup(0xFEBE, true, Some(0x24), Some(0x10));
            cpu.regset_mut().set_accumulator(0x07);
            set_amode_output(&mut cpu, Fetched {
                value: 0xC7,
                address: 0xAA,
            });

            let res_bit = bit(&mut cpu);

            assert_eq!(res_bit.ok(), Some(()));
            let regs = cpu.regset();
            assert_eq!(regs.zero(), false);
            assert_eq!(regs.negative(), true);
            assert_eq!(regs.overflowed(), true);
        }

        #[test]
        fn test_brk() {
            let mut cpu = setup(0xFEBE, true, Some(0x24), Some(0x10));
            cpu.regset_mut().set_accumulator(0x07);
            cpu.regset_mut().set_status(0xA1);
            set_amode_output(&mut cpu, Fetched {
                value: 0xC7,
                address: 0xAA,
            });
            cpu.writ_byte(BRK_VECTOR, 0xAA);
            cpu.writ_byte(BRK_VECTOR + 1, 0xAA);

            let res_brk = brk(&mut cpu);

            let saved_status = cpu.stk_pop();
            let lo = cpu.stk_pop();
            let hi = cpu.stk_pop();
            let saved_pc = Address::from_le_bytes([lo, hi]);

            assert_eq!(res_brk.ok(), Some(()));
            // 1 << 4 is the mask for the Brk bit
            // 1 << 2 is the mask for the Irq disabled bit
            assert_eq!(saved_status, 0xA1 | (1 << 4) | (1 << 2));
            assert_eq!(saved_pc, 0xFEBE + 1);

            let regs = cpu.regset();
            assert_eq!(regs.brk(), false);
            assert_eq!(regs.irq_disabled(), true);
            assert_eq!(cpu.pc(), 0xAAAA);
        }

        #[test]
        fn test_clear_flag_bits() {
            //
            // This test is for all "Clear ... flag" instructions
            // CLI, CLD, CLS, CLV
            // 
            
            let mut cpu = setup(0xFEBE, false, None, None);
            let regs = cpu.regset_mut();
            regs.set_carry(true);
            regs.set_irq_disabled(true);
            regs.set_decimal_mode(true);
            regs.set_overflowed(true);

            let res_clv = clv(&mut cpu);
            let res_cli = cli(&mut cpu);
            let res_cld = cld(&mut cpu);
            let res_clc = clc(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res_clc.ok(), Some(()));
            assert_eq!(res_clv.ok(), Some(()));
            assert_eq!(res_cli.ok(), Some(()));
            assert_eq!(res_cld.ok(), Some(()));
            assert_eq!(regs.carry(), false);
            assert_eq!(regs.overflowed(), false);
            assert_eq!(regs.decimal_mode(), false);
            assert_eq!(regs.irq_disabled(), false);
        }

        #[test]
        fn test_sta() {
            let mut cpu = setup(0xFEBE, true, Some(0x9D), Some(0x10 + 0x13));
            cpu.regset_mut().set_accumulator(0xAA);
            cpu.regset_mut().set_x_index(0x13);
            set_amode_output(&mut cpu, Fetched { value:0x12, address: 0x10 + 0x13 });
            let regs = cpu.regset_mut();

            let res = sta(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.accumulator(), 0xAA);
            let read_value = cpu.read_word(0x10 + 0x13);
            assert_eq!(read_value, 0xAA);
        }

        #[test]
        fn test_stx() {
            let mut cpu = setup(0xFEBE, true, Some(0x86), Some(0x10));
            cpu.regset_mut().set_x_index(0xBB);
            set_amode_output(&mut cpu, Fetched { value: 0x11, address: 0xEE });
            let regs = cpu.regset_mut();

            let res = stx(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.x_index(), 0xBB);
            let read_value = cpu.read_word(0xEE);
            assert_eq!(read_value, 0xBB);
        }

        #[test]
        fn test_sty() {
            let mut cpu = setup(0xFEBE, true, Some(0x84), Some(0x10));
            cpu.regset_mut().set_y_index(0xBB);
            set_amode_output(&mut cpu, Fetched { value: 0x11, address: 0xCD });

            let res = sty(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.y_index(), 0xBB);
            let read_value = cpu.read_word(0xCD);
            assert_eq!(read_value, 0xBB);
        }

        #[test]
        fn test_lda() {
            let mut cpu = setup(0xFEBE, true, Some(0xA9), Some(0x10));
            let regs = cpu.regset();

            set_amode_output(&mut cpu, ValueOnly(0x10));
            let res = lda(&mut cpu);
            let regs = cpu.regset();

            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.accumulator(), 0x10);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.zero(), false);

        }

        #[test]
        fn test_lda_negative() {
            let mut cpu = setup(0xFEBE, true, Some(0xA9), Some(0x10));

            set_amode_output(&mut cpu, Fetched {value: 0x80, address: 0xFE });
            let res = lda(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.accumulator(), 0x80);
            assert_eq!(regs.negative(), true);
            assert_eq!(regs.zero(), false);
        }

        #[test]
        fn test_ldx_zero() {
            let mut cpu = setup(0xFEBE, true, Some(0xA9), Some(0x10));
            set_amode_output(&mut cpu, Fetched {value: 0x00, address: 0xFE });

            let res = ldx(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.x_index(), 0x00);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.zero(), true);
        }

        #[test]
        fn test_ldy() {
            let mut cpu = setup(0xFEBE, true, Some(0xA9), Some(0x10));
            set_amode_output(&mut cpu, Fetched {value: 0x10, address: 0xFE });

            let res = ldy(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.y_index(), 0x10);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.zero(), false);
        }

        #[test]
        fn test_inx() {
            let mut cpu = setup(0xFEBE, true, Some(0xE8), None);
            cpu.regset_mut().set_x_index(0x09);
            set_amode_output(&mut cpu, ValueOnly(0x09));

            let res = inx(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.x_index(), 0xA);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.zero(), false);
        }

        #[test]
        fn test_inx_zero() {
            let mut cpu = setup(0xFEBE, true, Some(0xE8), None);
            cpu.regset_mut().set_x_index(0xFF);
            set_amode_output(&mut cpu, ValueOnly(0xFF));

            let res = inx(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.x_index(), 0x00);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.zero(), true);
        }

        #[test]
        fn test_inx_negative() {
            let mut cpu = setup(0xFEBE, true, Some(0xE8), None);
            cpu.regset_mut().set_x_index(0x7F);
            set_amode_output(&mut cpu, ValueOnly(0x7F));

            let res = inx(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.x_index(), 0x80);
            assert_eq!(regs.negative(), true);
            assert_eq!(regs.zero(), false);
        }

        #[test]
        fn test_inc() {
            let mut cpu = setup(0xFEBE, true, Some(0xE8), None);
            set_amode_output(&mut cpu, Fetched {value: 0x7E, address: 0xFE} );

            let res = inc(&mut cpu);

            let regs = cpu.regset();
            let read_value = cpu.read_byte(0xFE);

            assert_eq!(res.ok(), Some(()));
            assert_eq!(read_value, 0x7F);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.zero(), false);
        }

        #[test]
        fn test_inc_negative() {
            let mut cpu = setup(0xFEBE, true, Some(0xE8), None);
            set_amode_output(&mut cpu, Fetched {value: 0x7F, address: 0xFE} );

            let res = inc(&mut cpu);

            let regs = cpu.regset();
            let read_value = cpu.read_byte(0xFE);

            assert_eq!(res.ok(), Some(()));
            assert_eq!(read_value, 0x80);
            assert_eq!(regs.negative(), true);
            assert_eq!(regs.zero(), false);
        }

        #[test]
        fn test_inc_zero() {
            let mut cpu = setup(0xFEBE, true, Some(0xE8), None);
            set_amode_output(&mut cpu, Fetched {value: 0xFF, address: 0xFE} );

            let res = inc(&mut cpu);

            let regs = cpu.regset();
            let read_value = cpu.read_byte(0xFE);

            assert_eq!(res.ok(), Some(()));
            assert_eq!(read_value, 0x00);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.zero(), true);
        }
        
        #[test]
        fn test_do_compare_greater_than() {
            let mut regset = RegisterSet::default();
            let lhs: Byte = 14;
            let rhs: Byte = 12;

            do_compare(&mut regset, lhs, rhs);

            assert_eq!(regset.carry(), true);
            assert_eq!(regset.negative(), false);
            assert_eq!(regset.zero(), false);
        }

        #[test]
        fn test_do_compare_equal() {
            let mut regset = RegisterSet::default();
            let lhs: Byte = 10;
            let rhs: Byte = 10;

            do_compare(&mut regset, lhs, rhs);

            assert_eq!(regset.carry(), true);
            assert_eq!(regset.negative(), false);
            assert_eq!(regset.zero(), true);
        }

        #[test]
        fn test_do_compare_less_than() {
            let mut regset = RegisterSet::default();
            let lhs: Byte = 1;
            let rhs: Byte = 10;

            do_compare(&mut regset, lhs, rhs);

            assert_eq!(regset.carry(), false);
            assert_eq!(regset.negative(), true);
            assert_eq!(regset.zero(), false);
        }

        #[test]
        fn test_cmp() {
            let mut cpu = setup(0xFEBE, true, Some(0xD5), None);
            cpu.regset_mut().set_accumulator(0x20);
            set_amode_output(&mut cpu, Fetched { value: 0x14, address: 0xAE } );

            let res = cmp(&mut cpu);

            let regs = cpu.regset();

            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.carry(), true);
            assert_eq!(regs.negative(), false);
            assert_eq!(regs.zero(), false);
        }

        #[test]
        fn test_rti() {
            let mut cpu = setup(0xFEBE, true, Some(0x40), None);
            set_amode_output(&mut cpu, ValueOnly(0x00));
            cpu.regset_mut().set_stk_ptr(0xFA);
            cpu.regset_mut().set_status(0x00);
            cpu.writ_byte(0x01FB, 0xF1);
            cpu.writ_byte(0x01FC, 0x01);
            cpu.writ_byte(0x01FD, 0x40);

            let res = rti(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.prog_counter(), 0x4001);
            assert_eq!(regs.stk_ptr(), 0xFD);
            assert_eq!(regs.negative(), true);
            assert_eq!(regs.zero(), false);
            assert_eq!(regs.brk(), false);
            assert_eq!(regs.unused(), false);
            assert_eq!(regs.status(), 0xC1);
        }

        #[test]
        fn test_rts() {
            let mut cpu = setup(0xFEBE, true, Some(0x40), None);
            set_amode_output(&mut cpu, ValueOnly(0x00));
            cpu.regset_mut().set_stk_ptr(0xFB);
            cpu.regset_mut().set_status(0x00);
            cpu.writ_byte(0x01FC, 0x12);
            cpu.writ_byte(0x01FD, 0x21);

            let res = rts(&mut cpu);

            let regs = cpu.regset();
            assert_eq!(res.ok(), Some(()));
            assert_eq!(regs.prog_counter(), 0x2113);
            assert_eq!(regs.stk_ptr(), 0xFD);
        }

    }
}

///
/// Addressing modes
/// The 6502 cpu support different kinds of _addressing mode_/
///
///
mod m6502_addressing_modes {
    use super::{Address, Byte, Opcode, Word};
    use super::{AddressingOutput, AddressingOutput::*, CpuError, CpuError::*};
    use super::{Cpu, Instruction, MainBus};

    use crate::mos6502::{AddressingMode, AddressingModeFn};
    use std::cell::RefCell;
    use std::rc::Rc;

    #[inline]
    pub fn to_fun(amode: AddressingMode) -> AddressingModeFn {
        return match amode {
            AddressingMode::Imp => implied_am,
            AddressingMode::Imm => immediate_am,
            AddressingMode::Zp0 => zeropage_am,
            AddressingMode::Zpx => zeropage_x_am,
            AddressingMode::Zpy => zeropage_y_am,
            AddressingMode::Abs => absolute_am,
            AddressingMode::Abx => absolute_x_am,
            AddressingMode::Aby => absolute_y_am,
            AddressingMode::Ind => indirect_am,
            AddressingMode::Iny => indirect_y_am,
            AddressingMode::Inx => indirect_x_am,
            AddressingMode::Rel => relative_am,
        };
    }

    ///
    /// All addressing mode functions **require** that
    /// the cpu has loaded a value for `current_instruction`
    /// and its concrete operands. Each of them will check
    /// for the operands they expect, but that there is
    /// a `current_instruction`. A `panic!()` (or appropriate
    /// error return) will occur if **some** of these
    /// requirements are not met.
    ///
    /// ---------------------
    ///
    /// **Implied**
    /// This addressing mode is the simplest one, because it is
    /// use only with instructions which do not need any additional
    /// context -- it is _implied_.
    //
    //  No operands here. This addressing mode does not use any
    //  additional values. In this current implementation
    //  the accumulator addressing mode is "simulated" by
    //  calling `implied_am`. The only difference between the
    //  two is that the accumulator AM is using the
    //  value of the accumulator as its operand, hence the name --
    //  accumulator. That is why the contents of the
    //  accumulator are set as this addressing mode's output.
    //
    pub fn implied_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let accumulator = cpu.regset().accumulator();
        let fetched = ValueOnly(accumulator);

        Ok(fetched)
    }

    ///
    /// **Immediate**
    /// This addressing mode is used when the next byte
    /// is directly used as a value.
    ///
    pub fn immediate_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let i = cpu.i.as_ref().unwrap();
        if let Some(operand) = i.operand {
            let fetched = ValueOnly(operand as u8);
            Ok(fetched)
        } else {
            Err(ExpectedOperandMissing)
        }
    }

    ///
    /// **Zero page**
    ///
    /// In order to utilize the system capabilities more efficiently
    /// it is possible to address _absolutely_ the first page of
    /// memory _faster_ by providing a 8-bit number instead of 16-bit
    /// for address value (6502 addresses are 16-bit).
    ///
    pub fn zeropage_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        return match read_from_operand_with_offset(cpu, 0, true) {
            Ok((address, value)) => Ok(Fetched {
                value: value,
                address: address,
            }),
            Err(e) => Err(e),
        };
    }

    ///
    /// **Zero page with X offset**
    ///
    /// Same as Zero page but indexes using the value inside
    /// the X-index register. If the address is bigger than
    /// the maximum of zero page, it wrapps around.
    ///
    pub fn zeropage_x_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let offset = cpu.regset().x_index() as Address;
        return match read_from_operand_with_offset(cpu, offset, true) {
            Ok((address, value)) => Ok(Fetched {
                value: value,
                address: address,
            }),
            Err(e) => Err(e),
        };
    }

    ///
    /// **Zero page with Y offset**
    ///
    /// Same as Zero page with X offset but using the value
    /// inside the Y-index register instead of the X-index.
    ///
    pub fn zeropage_y_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let offset = cpu.regset().y_index() as Address;
        return match read_from_operand_with_offset(cpu, offset, true) {
            Ok((address, value)) => Ok(Fetched {
                value: value,
                address: address,
            }),
            Err(e) => Err(e),
        };
    }

    /// Helper routine used from zero-page and
    /// absolute addressing modes (includeing
    /// the x-index and y-index offset ones).
    /// Interpret the given operand as an adress
    /// and read from it a single byte value.
    fn read_from_operand_with_offset(
        cpu: &mut Cpu,
        offset: Address,
        zeropage: bool,
    ) -> Result<(Address, Byte), CpuError> {
        let i = cpu.i.as_ref().unwrap();
        if let Some(mut operand) = i.operand {
            operand += offset;
            if zeropage {
                operand &= 0x00FF;
            } else {
                if operand & 0xFF00 != (operand - offset) & 0xFF00 {
                    mark_extra_clockcycle(cpu);
                }
            }
            return Ok((operand, cpu.read_byte(operand)));
        }
        Err(ExpectedOperandMissing)
    }

    ///
    /// **Absolute**
    /// Specifies the memory location explicitly in the two bytes
    /// following the opcode. In order to fetch the value which
    /// is going to be used, the full 16-bit address has to be
    /// acquired and then read from.
    ///
    pub fn absolute_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        return match read_from_operand_with_offset(cpu, 0, false) {
            Ok((address, value)) => Ok(Fetched {
                value: value,
                address: address,
            }),
            Err(e) => Err(e),
        };
    }

    ///
    /// **Absolute with X offset**
    ///
    /// Analogical to the absolute addressing mode but
    /// uses the value of the X-index register as an offset
    /// before actually reading the value.
    pub fn absolute_x_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let offset = cpu.regset().x_index() as Address;
        return match read_from_operand_with_offset(cpu, offset, false) {
            Ok((address, value)) => Ok(Fetched {
                value: value,
                address: address,
            }),
            Err(e) => Err(e),
        };
    }

    ///
    /// **Absolute with Y offset**
    ///
    /// Same as the absolute with X offset but uses the
    /// Y-index register instead.
    ///
    /// FIXME: Consider extracting common logic with `absolute_x_am`
    /// into a helping routine.
    pub fn absolute_y_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let offset = cpu.regset().y_index() as Address;
        return match read_from_operand_with_offset(cpu, offset, false) {
            Ok((address, value)) => Ok(Fetched {
                value: value,
                address: address,
            }),
            Err(e) => Err(e),
        };
    }

    ///
    /// **Relative**
    ///
    /// The supplied byte is interpreted as a _signed offset_
    /// This offset is then added to the program counter.
    ///
    pub fn relative_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let i = cpu.i.as_ref().unwrap();
        if let Some(operand) = i.operand {
            let operand_u8 = operand as u8;
            let value = cpu.pc().wrapping_add(signedbyte_to_word(operand_u8));
            return Ok(AbsoluteAddress(value));
        }

        Err(ExpectedOperandMissing)
    }

    fn signedbyte_to_word(p_num: Byte) -> Word {
        let num = Word::from(p_num);
        return if num & 0x80 != 0 { num | 0xFF00 } else { num };
    }

    ///
    /// **Indirect**\
    /// \
    /// The supplied 16-bit address is set as a value for the
    /// program counter.
    /// \
    /// **NB:**\
    /// This operation has a hardware bug when
    /// a page boundary is crossed.
    pub fn indirect_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let i = cpu.i.as_ref().unwrap();
        if let Some(ptr) = i.operand {
            // Simulate hardware bug
            let page_crossed = (ptr & 0x00FF) == 0x00FF;
            let address_of_next_hi = if page_crossed { ptr & 0xFF00 } else { ptr + 1 };

            let next_address_lo = cpu.read_byte(ptr);
            let next_address_hi = cpu.read_byte(address_of_next_hi);
            let next_address = Address::from_le_bytes([next_address_lo, next_address_hi]);

            return Ok(AbsoluteAddress(next_address));
        }

        Err(ExpectedOperandMissing)
    }

    ///
    /// **Indirect with X-index offset**
    ///
    /// A 8-bit address is suplied. It is then offset
    /// by the value of the X-index register to a
    /// location in the zero page. Then the actual
    /// address is read.
    pub fn indirect_x_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let i = cpu.i.as_ref().unwrap();

        if let Some(base_u16) = i.operand {
            let base = base_u16 as Byte;
            let x_index = cpu.regset().x_index();
            let offset = Address::from(base.wrapping_add(x_index));

            let lo = cpu.read_byte(offset);
            let hi = cpu.read_byte((offset + 1) & 0x00FF);

            let next_address = Address::from_le_bytes([lo, hi]);

            return Ok(Fetched {
                value: cpu.read_byte(next_address),
                address: next_address,
            });
        }

        Err(ExpectedOperandMissing)
    }

    pub fn indirect_y_am(cpu: &mut Cpu) -> Result<AddressingOutput, CpuError> {
        let i = cpu.i.as_ref().unwrap();

        if let Some(base_u16) = i.operand {
            let lo = cpu.read_byte(base_u16);
            let hi = cpu.read_byte((base_u16 + 1) & 0x00FF);
            let next_address = Address::from_le_bytes([lo, hi]);

            let y_index = cpu.regset().y_index();
            let offset_address = next_address.wrapping_add(y_index as Address);

            if next_address & 0xFF00 != offset_address & 0xFF00 {
                mark_extra_clockcycle(cpu);
            }

            return Ok(Fetched {
                value: cpu.read_byte(offset_address),
                address: offset_address,
            });
        }

        Err(ExpectedOperandMissing)
    }

    fn mark_extra_clockcycle(cpu: &mut Cpu) {
        *cpu.time_mut().residual_mut() += 1;
    }

    #[cfg(test)]
    mod test {
        use super::*;

        /// A setup() routine for all tests.
        fn setup(custom_pc: Word, connect: bool, opcode: Opcode, operand: Option<Word>) -> Cpu {
            let mut cpu = Cpu::new_custompc(custom_pc);

            cpu.i = Some(Instruction::decode_by(opcode));
            cpu.i.as_mut().unwrap().operand = operand;

            if connect {
                cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
            }

            cpu
        }

        #[test]
        fn test_implied_am() {
            let mut cpu = setup(0x0000, false, 0x08, None);
            cpu.regset_mut().set_accumulator(0x1A);

            let result = implied_am(&mut cpu);

            assert_eq!(result.ok(), Some(ValueOnly(0x1A)));
            assert_eq!(cpu.pc(), 0x0000);
        }

        #[test]
        fn test_immediate_am() {
            let mut cpu = setup(0x0000, true, 0xA9, Some(0x10));
            cpu.writ_byte(0xFA, 0x10);

            let result = immediate_am(&mut cpu);

            assert_eq!(result.ok(), Some(ValueOnly(0x10)));
            assert_eq!(cpu.pc(), 0x0000);
        }

        #[test]
        fn test_zeropage_am() {
            let mut cpu = setup(0x0000, true, 0x25, Some(0x35));
            cpu.writ_byte(0x35, 0x10);

            let result = zeropage_am(&mut cpu);

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x10,
                    address: 0x35
                })
            );
        }

        #[test]
        fn test_zeropage_x_offset_am() {
            let mut cpu = setup(0x00, true, 0x35, Some(0x35));
            *cpu.regset_mut().x_index_mut() = 3;
            cpu.writ_byte(0x35 + 3, 0x10);

            let result = zeropage_x_am(&mut cpu);

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x10,
                    address: 0x35 + 3
                })
            );
        }

        #[test]
        fn test_zeropage_offset_wrapping_am() {
            let mut cpu = setup(0x00, true, 0x35, Some(0x35));
            *cpu.regset_mut().y_index_mut() = 0xff;
            cpu.writ_byte(0x35 - 1, 0x10);

            let result = zeropage_y_am(&mut cpu);

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x10,
                    address: 0x35 - 1
                })
            );
        }

        #[test]
        fn test_absolute_am() {
            let mut cpu = setup(0x00, true, 0x2D, Some(0x0210));
            cpu.writ_byte(0x0210, 0x10);

            let result = absolute_am(&mut cpu);

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x10,
                    address: 0x210
                })
            );
        }

        #[test]
        fn test_absolute_offset_am() {
            let mut cpu = setup(0x00, true, 0x3D, Some(0x0210));
            *cpu.regset_mut().y_index_mut() = 0xA;
            cpu.writ_byte(0x0210 + 0xA, 0x10);

            let result = absolute_y_am(&mut cpu);

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x10,
                    address: 0x210 + 0xA
                })
            );
        }

        #[test]
        fn test_relative_am_w_positive_number() {
            let mut cpu = setup(0x00, true, 0x30, Some(0x10));
            cpu.writ_byte(0x10, 0x10);

            let result = relative_am(&mut cpu);

            assert_eq!(result.ok(), Some(AbsoluteAddress(0x10)));
        }

        #[test]
        fn test_relative_am_w_negative_number() {
            let mut cpu = setup(0x90, true, 0x30, Some(0x80));
            cpu.writ_byte(0x10, 0x10);

            let result = relative_am(&mut cpu);

            assert_eq!(result.ok(), Some(AbsoluteAddress(0x10)));
        }

        #[test]
        fn test_indirect_am_page_cross() {
            let mut cpu = setup(0x00, true, 0x6C, Some(0x1011));
            cpu.writ_byte(0x1011, 0x01);
            cpu.writ_byte(0x1012, 0xFF);

            let result = indirect_am(&mut cpu);

            assert_eq!(result.ok(), Some(AbsoluteAddress(0xFF01)));
        }

        #[test]
        fn test_indirect_am() {
            let mut cpu = setup(0x00, true, 0x6C, Some(0x10FF));
            cpu.writ_byte(0x10FF, 0x01);
            cpu.writ_byte(0x1000, 0xA7);

            let result = indirect_am(&mut cpu);

            assert_eq!(result.ok(), Some(AbsoluteAddress(0xA701)));
        }

        #[test]
        fn test_indirect_xoffset_am_1() {
            let mut cpu = setup(0x00, true, 0x21, Some(0x20));
            *cpu.regset_mut().x_index_mut() = 0x04;
            cpu.writ_byte(0x24, 0x74);
            cpu.writ_byte(0x25, 0x20);
            cpu.writ_byte(0x2074, 0x10);

            let result = indirect_x_am(&mut cpu);

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x10,
                    address: 0x2074
                })
            );
        }

        #[test]
        fn test_indirect_xoffset_am_2() {
            let mut cpu = setup(0x00, true, 0x21, Some(0x25));
            *cpu.regset_mut().x_index_mut() = 0x10;
            cpu.writ_byte(0x35, 0x01);
            cpu.writ_byte(0x36, 0xA7);
            cpu.writ_byte(0xA701, 0x19);

            let result = indirect_x_am(&mut cpu);

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x19,
                    address: 0xA701
                })
            );
        }

        #[test]
        fn test_indirect_xoffset_am_3() {
            let mut cpu = setup(0x00, true, 0x21, Some(0xFE));
            *cpu.regset_mut().x_index_mut() = 0x01;
            cpu.writ_byte(0x0000, 0xFE);
            cpu.writ_byte(0x00FF, 0x01);
            cpu.writ_byte(0xFE01, 0x17);

            let result = indirect_x_am(&mut cpu);

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x17,
                    address: 0xFE01
                })
            );
        }

        #[test]
        fn test_indirect_yoffset_am_1() {
            let mut cpu = setup(0x00, true, 0x31, Some(0x25));
            *cpu.regset_mut().y_index_mut() = 0x10;
            cpu.writ_byte(0x0025, 0xFF);
            cpu.writ_byte(0x0026, 0xA7);
            cpu.writ_byte(0xA80F, 0x34);

            let result = indirect_y_am(&mut cpu);
            let marked_extra_cycle = cpu.time.residual == 1;

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x34,
                    address: 0xA80F
                })
            );
            assert_eq!(marked_extra_cycle, true);
        }

        #[test]
        fn test_indirect_yoffset_am_2() {
            let mut cpu = setup(0x00, true, 0x31, Some(0x25));
            *cpu.regset_mut().y_index_mut() = 0x10;
            cpu.writ_byte(0x0025, 0x01);
            cpu.writ_byte(0x0026, 0xA7);
            cpu.writ_byte(0xA711, 0x34);

            let result = indirect_y_am(&mut cpu);
            let marked_extra_cycle = cpu.time.residual == 1;

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x34,
                    address: 0xA711
                })
            );
            assert_eq!(marked_extra_cycle, false);
        }

        #[test]
        fn test_indirect_yoffset_am_3() {
            let mut cpu = setup(0x00, true, 0x31, Some(0x86));
            *cpu.regset_mut().y_index_mut() = 0x10;
            cpu.writ_byte(0x0086, 0x28);
            cpu.writ_byte(0x0087, 0x40);
            cpu.writ_byte(0x4038, 0x37);

            let result = indirect_y_am(&mut cpu);
            let marked_extra_cycle = cpu.time.residual == 1;

            assert_eq!(
                result.ok(),
                Some(Fetched {
                    value: 0x37,
                    address: 0x4038
                })
            );
            assert_eq!(marked_extra_cycle, false);
        }
    }
}

///
/// Utilities
///
/// Some additional functions needed for the cpu
///

#[derive(Debug, PartialEq)]
pub struct Asm {
    code: Vec<Instruction>,
}

impl Asm {
    pub fn from_addr_range(cpu: &mut Cpu, begin_address: Address, limit: u16) -> Asm {
        let mut code: Vec<Instruction> = Vec::new();

        let end_address = begin_address + limit;
        let mut address = begin_address;
        while address < end_address {
            let opcode = cpu.read_byte(address);
            let mut i = Instruction::decode_by(opcode);
            load_operand(cpu, &mut i, address);
            address += i.size;
            code.push(i);
        }

        Asm { code }
    }

    pub fn stringify_range(
        cpu: &mut Cpu,
        begin_address: Address,
        limit: u16,
    ) -> Result<String, ()> {
        let asm = Asm::from_addr_range(cpu, begin_address, limit);
        asm.stringify(true, true)
    }

    pub fn stringify(&self, address_column: bool, addressing_mode: bool) -> Result<String, ()> {
        let mut res = String::new();
        for i in self.code.iter() {
            res += &i.to_string();
        }

        if res.len() > 0 {
            return Ok(res);
        }

        Err(())
    }
}

const STACK_OFFSET: Address = 0x100;

impl Cpu {

    fn stk_ptr_inc(&mut self) -> u8 {
        let sptr = self.regset_mut().stk_ptr_mut();
        *sptr = sptr.wrapping_add(1);
        *sptr
    }

    fn stk_ptr_dec(&mut self) -> u8 {
        let stk_ptr = self.regset().stk_ptr();
        *self.regset_mut().stk_ptr_mut() = stk_ptr.wrapping_sub(1);
        stk_ptr
    }

    /// **stk_push()** - Pushes a byte to the stack stored in memory with offset `STACK_OFFSET`.
    /// Note that this routine will fail if no interface is connected.
    fn stk_push(&mut self, data: Byte) {
        let stk_ptr = self.stk_ptr_dec();
        let addr = STACK_OFFSET + Address::from(stk_ptr);
        self.writ_byte(addr, data);
    }

    fn stk_doublepush(&mut self, data: Word) {
        self.stk_push((data >> 8) as u8);
        self.stk_push((data) as u8);
    }

    /// **stk_pop()** - Pops a byte from the stack stored in memory with offset `STACK_OFFSET`.
    /// **NB:** This routine will fail if no 2 passed; 0 failinterface is connected.
    fn stk_pop(&mut self) -> Byte {
        let stk_ptr = self.stk_ptr_inc();
        let addr = STACK_OFFSET + Address::from(stk_ptr);
        let data = self.read_byte(addr);
        data
    }

    /// **disassemble()** - Given a beginning address, disassemble `limit` of bytes from memory
    /// matching them to Instruction instances.
    pub fn disassemble(&mut self, begin: Address, limit: Address) -> Option<Asm> {
        if self.bus_conn.is_none() {
            return None;
        }

        let asm = Asm::from_addr_range(self, begin, limit);
        if asm.code.len() > 0 {
            return Some(asm);
        }

        None
    }

    pub fn print_disassembly(&mut self, begin: Address, limit: Address) {
        if let Some(disassembly) = self.disassemble(begin, limit) {
            for i in disassembly.code.iter() {}
        }
    }

    /// **load_program()** - Given a vector of bytes, store `limit` of them into memory
    /// starting from `begin` in memory.
    fn load_program(
        &mut self,
        program: &Vec<Byte>,
        begin: Address,
        limit: u16,
        start_it: bool,
    ) -> Result<Address, CpuError> {
        if self.bus_conn.is_none() {
            return Err(CpuError::BusInterfaceMissing);
        }

        let end = begin + limit;
        for address in begin..end {
            let index = usize::from(address - begin);
            self.writ_byte(address, program[index]);
        }

        let saved_pc = self.pc();
        if start_it {
            *self.regset_mut().prog_counter_mut() = begin;
        }

        Ok(saved_pc)
    }

    /// **load_file()**  - Given a filename of a binary source file, load the data in memory
    /// starting from address `begin`. If `start_it` is true, the program counter should
    /// be set to `begin` and execute the code.
    fn load_file(
        &mut self,
        filename: &str,
        begin: Address,
        start_it: bool,
    ) -> Result<Address, CpuError> {
        let mut program: Vec<Byte> = Vec::new();
        if let Ok(mut file) = File::open(filename) {
            if let Ok(_) = file.read_to_end(&mut program) {
                return self.load_program(&program, begin, program.len() as u16, start_it);
            }
        }

        Err(CpuError::FailedLoadingProgram)
    }
}

/// **load_operand_curr_i()** - This is called right before
/// the addressing mode specifics are executed in order
/// to fetch the required operand into the operand
/// field in `i`.
pub fn load_operand_curr_i(cpu: &mut Cpu) {
    use AddressingMode::*;

    if cpu.i.is_none() {
        return;
    }

    // The instruction is already loaded since we are looking at it
    // The reason we are updating this value here and not beforehand
    // is sa that a support could be provided for `load_operand()`
    // also a.k.a so that we can load operands for instructions
    // which are not the current one whici is being executed.
    let loaded_from: Address = cpu.pc() - 1;

    let num_fetched = match cpu.i.as_ref().unwrap().amode {
        Imp => 0,
        Imm | Zp0 | Zpx | Zpy | Inx | Iny | Rel => 1,
        Abs | Abx | Aby | Ind => 2,
    };

    let operand = match num_fetched {
        0 => None,
        1 => Some(Word::from(cpu.fetch())),
        2 => {
            let lo = cpu.fetch();
            let hi = cpu.fetch();
            Some(Word::from_le_bytes([lo, hi]))
        }
        _ => unreachable!("Unknown number of bytes for operand"),
    };

    let i = cpu.i.as_mut().unwrap();
    i.loaded_from = loaded_from;
    i.operand = operand;
}

/// **load_operand()** - For any given instruction (only the addressing mode
/// is actually of importance here, fetch any operands that the instruction
/// requires taking into account that the address of the instruction in memory
/// is also provided.
pub fn load_operand(cpu: &mut Cpu, i: &mut Instruction, address: Address) {
    // Store previous state
    let saved_pc = cpu.pc();
    // The instruction has already been fetched
    // so if address is where the instruction is
    // then address + 1 is where the operand is.
    *cpu.regset_mut().prog_counter_mut() = address.wrapping_add(1);

    let saved_i = cpu.i.clone();
    cpu.i.replace(i.clone());

    load_operand_curr_i(cpu);

    i.clone_from(&cpu.i.as_ref().unwrap());

    // Restore previous state
    cpu.i.clone_from(&saved_i);
    *cpu.regset_mut().prog_counter_mut() = saved_pc;
}

#[cfg(test)]
mod test {
    use super::AddressingOutput::*;
    use super::*;

    #[test]
    fn test_create_cpu() {
        let _ = Cpu::new();
        assert!(true);
    }

    #[test]
    fn test_getters_fields() {
        let mut regset = RegisterSet::new();
        assert_eq!(regset.accumulator(), 0);
        assert_eq!(regset.stk_ptr(), 0xfd);
        regset.set_accumulator(2);
        assert_eq!(regset.accumulator(), 2);
    }

    #[test]
    fn test_getters_and_setters_bits() {
        let mut regset = RegisterSet::default();
        let carry_is_set: bool = regset.carry();
        assert_eq!(carry_is_set, false);

        regset.set_carry(true);
        let carry_is_set: bool = regset.carry();
        assert_eq!(carry_is_set, true);
    }

    #[test]
    fn test_create_bus() {
        let bus = MainBus::new();
        assert_eq!(bus.mem[0], 0x0);
    }

    #[test]
    fn test_cpu_with_host() {
        let mut bus = MainBus::new();
        bus.write(1, 10);
        let mut cpu = Cpu::new();
        cpu.connect_to(Rc::new(RefCell::new(bus)));

        let retval1 = cpu.read_byte(1);
        let retval2 = cpu.read_byte(2);
        assert_eq!(retval1, 10);
        assert_eq!(retval2, 0);
    }

    #[test]
    fn test_stk_operations() {
        let mut cpu = Cpu::new_connected(Some(Rc::new(RefCell::new(MainBus::new()))));

        cpu.stk_push(0xcd);
        let cd = cpu.stk_pop();
        assert_eq!(cd, 0xcd);

        cpu.stk_push(0xab);
        let ab = cpu.stk_pop();
        assert_eq!(ab, 0xab);
    }

    #[test]
    fn test_stk_operations_double() {
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
    fn test_reset_cpu() {
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
    fn test_read_seq_from_host() {
        let cpu = Cpu::new_connected(Some(Rc::new(RefCell::new(MainBus::new()))));
        let result = cpu.read_some(0x000, 0x7ff);
        let expected_result = vec![0x00; 0x7ff];
        assert_eq!(expected_result, result);
    }

    #[test]
    fn test_decode_by_correct() {
        use super::m6502_addressing_modes::relative_am;
        use super::m6502_intruction_set::bpl;

        let cpu = Cpu::new_connected(Some(Rc::new(RefCell::new(MainBus::new()))));
        let i = Instruction::decode_by(0x10);

        assert_eq!(i.mnemonic, "bpl".to_string());
        assert_eq!(i.time, 2);
        assert_eq!(i.size, 2);
        assert_eq!(i.fun as usize, bpl as usize);
        assert_eq!(i.amode_fun as usize, relative_am as usize);
    }

    #[test]
    fn test_load_program() {
        let mut cpu = Cpu::new_custompc(0x1000);
        cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

        let prog: Vec<Byte> = vec![
            162, 10, 142, 0, 0, 162, 3, 142, 1, 0, 172, 0, 0, 169, 0, 24, 109, 1, 0, 136, 208, 250,
            141, 2, 0, 234, 234, 234,
        ];

        // Loads this program (assembled [here](https://www.masswerk.at/6502/assembler.html))
        //
        // *=$8000 ; set PC=0x8000
        // LDX #10
        // STX $0000
        // LDX #3
        // STX $0001
        // LDY $0000
        // LDA #0
        // CLC
        // loop
        // ADC $0001
        // DEY
        // BNE loop
        // STA $0002
        // NOP
        // NOP
        // NOP

        let len = prog.len() as u16;

        let old_pc = cpu.load_program(&prog, 0x8000, len, true);
        assert_eq!(old_pc.ok(), Some(0x1000));

        let read = cpu.read_some(0x8000, len);
        assert_eq!(read, prog);
    }

    #[test]
    fn test_load_program_and_disassemble() {
        let mut cpu = Cpu::new_custompc(0x1000);
        cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
        let prog: Vec<Byte> = vec![162, 10, 142, 0, 0, 162, 3];
        let old_pc = cpu.load_program(&prog, 0x8000, 7, true);
        let expected_asm = Asm {
            code: vec![
                Instruction::decode_by(162),
                Instruction::decode_by(142),
                Instruction::decode_by(162),
            ],
        };

        let asm = cpu.disassemble(0x8000, 7);

        assert_eq!(old_pc, Ok(0x1000));
        assert_eq!(asm, Some(expected_asm));
    }

    #[test]
    fn test_load_binary_file_program() {
        let mut cpu = Cpu::new_custompc(0x0000);
        cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

        let expected_asm = Asm {
            code: vec![
                Instruction::decode_by(0xA9),
                Instruction::decode_by(0x85),
                Instruction::decode_by(0xA9),
            ],
        };

        let old_pc = cpu.load_file("src/test.bin", 0x8000, true);

        let asm = cpu.disassemble(0x8000, 6);
        assert_eq!(old_pc, Ok(0x0000));
        assert_eq!(asm, Some(expected_asm));
    }

    #[test]
    fn test_single_clock_cycle() {
        let mut cpu = Cpu::new_custompc(0x0000);
        cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

        let wrapped_old_pc = cpu.load_file("src/test.bin", 0x8000, true);

        cpu.clock_cycle();

        assert_eq!(
            cpu.i.as_ref().unwrap().amode_output,
            AddressingOutput::ValueOnly(0x02)
        );
        assert_eq!(cpu.pc(), 0x8002);
        assert_eq!(wrapped_old_pc.ok(), Some(0x0000));
    }

    #[test]
    fn test_handle_irq_correct() {
        let mut cpu = Cpu::new_custompc(0x0000);
        cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
        cpu.writ_byte(0xFFFE, 0x00);
        cpu.writ_byte(0xFFFF, 0x20);
        cpu.writ_byte(0x2000, 0xA9);
        cpu.writ_byte(0x2001, 0x10);
        cpu.regset_mut().set_irq_disabled(false);

        let success = cpu.inthandle(Irq);
        // irq cycles
        for _ in 0..7 {
            cpu.clock_cycle();
        }
        // "lda #10" cycles
        for _ in 0..2 {
            cpu.clock_cycle();
        }

        assert_eq!(success, true);
        assert_eq!(cpu.time.residual, 0);
        assert_eq!(cpu.pc(), 0x2002);
        assert_eq!(cpu.regset.irq_disabled(), true);
    }

    #[test]
    fn test_handle_irq_incorrect() {
        let mut cpu = Cpu::new_custompc(0x0000);
        cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
        cpu.writ_byte(0xFFFE, 0x00);
        cpu.writ_byte(0xFFFF, 0x20);
        cpu.writ_byte(0x2000, 0xA9);
        cpu.writ_byte(0x2001, 0x10);
        cpu.regset_mut().set_irq_disabled(true);

        let success = cpu.inthandle(InterruptKind::Irq);

        assert_eq!(success, false);
        assert_eq!(cpu.time.residual, 0);
        assert_eq!(cpu.pc(), 0x0000);
        assert_eq!(cpu.regset.irq_disabled(), true);
    }

    #[test]
    fn test_handle_nmi() {
        let mut cpu = Cpu::new_custompc(0x0000);
        cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
        cpu.writ_byte(0xFFFA, 0x00);
        cpu.writ_byte(0xFFFB, 0x20);
        cpu.writ_byte(0x2000, 0xA9);
        cpu.writ_byte(0x2001, 0x10);
        cpu.regset_mut().set_irq_disabled(true);

        let success = cpu.inthandle(InterruptKind::Nmi);
        // irq cycles
        for _ in 0..8 {
            cpu.clock_cycle();
        }
        // "lda #10" cycles
        for _ in 0..2 {
            cpu.clock_cycle();
        }

        assert_eq!(success, true);
        assert_eq!(cpu.time.residual, 0);
        assert_eq!(cpu.pc(), 0x2002);
        assert_eq!(cpu.regset.irq_disabled(), true);
    }

    fn setup(custom_pc: Word, connect: bool, opcode: Opcode, operand: Option<Word>) -> Cpu {
        let mut cpu = Cpu::new_custompc(custom_pc);

        cpu.i = Some(Instruction::decode_by(opcode));
        cpu.i.as_mut().unwrap().operand = operand;

        if connect {
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
        }

        cpu
    }

    #[test]
    fn test_prepare_operands_zero() {
        let mut cpu = setup(0x0001, true, 0x00, None);
        cpu.i = Some(Instruction::decode_by(0x08));

        load_operand_curr_i(&mut cpu);

        let i = cpu.i.unwrap();
        assert_eq!(i.operand, None);
        assert_eq!(i.loaded_from, 0x0);
    }

    #[test]
    fn test_prepare_operands_one() {
        let mut cpu = setup(0x001, true, 0x00, None);
        cpu.i = Some(Instruction::decode_by(0xA9));
        cpu.writ_byte(0x0001, 0x10);

        load_operand_curr_i(&mut cpu);

        assert_eq!(cpu.pc(), 0x02);
        let i = cpu.i.unwrap();
        assert_eq!(i.operand.is_some(), true);
        assert_eq!(i.operand.unwrap(), 0x10);
        assert_eq!(i.loaded_from, 0x0);
    }

    #[test]
    fn test_prepare_operands_two() {
        let mut cpu = setup(0x001, true, 0x00, None);
        cpu.i = Some(Instruction::decode_by(0xAD));
        cpu.writ_byte(0x1, 0x10);
        cpu.writ_byte(0x2, 0x11);

        load_operand_curr_i(&mut cpu);

        assert_eq!(cpu.pc(), 0x03);
        let i = cpu.i.unwrap();
        assert_eq!(i.operand.is_some(), true);
        assert_eq!(i.operand.unwrap(), 0x1110);
        assert_eq!(i.loaded_from, 0x0);
    }

    #[test]
    fn test_prepare_operands_zero_custom_i() {
        let mut cpu = setup(0xFEBE, true, 0xA9, Some(0x10));
        cpu.writ_byte(0x1002, 0xEE);
        cpu.writ_byte(0x1003, 0xFF);
        let mut i = Instruction::decode_by(0x08);
        load_operand(&mut cpu, &mut i, 0x1001);

        assert_eq!(cpu.pc(), 0xFEBE);
        assert_eq!(cpu.i, Some(Instruction::decode_by(0xA9)));
        assert_eq!(i.operand, None);
        assert_eq!(i.amode_output, NotExecuted);
        assert_eq!(i.loaded_from, 0x1001);
    }

    #[test]
    fn test_prepare_operands_one_custom_i_1() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1002, 0x10);
        let mut i = Instruction::decode_by(0xA9);

        load_operand(&mut cpu, &mut i, 0x1001);

        assert_eq!(cpu.pc(), 0xFEBE);
        assert_eq!(cpu.i, Some(Instruction::decode_by(0x08)));
        assert_eq!(i.operand, Some(0x10));
        assert_eq!(i.amode_output, NotExecuted);
        assert_eq!(i.loaded_from, 0x1001);
    }

    #[test]
    fn test_prepare_operands_one_custom_i_2() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1002, 0x20);
        let mut i = Instruction::decode_by(0x19);

        load_operand(&mut cpu, &mut i, 0x1001);

        assert_eq!(cpu.pc(), 0xFEBE);
        assert_eq!(cpu.i, Some(Instruction::decode_by(0x08)));
        assert_eq!(i.operand, Some(0x20));
        assert_eq!(i.amode_output, NotExecuted);
        assert_eq!(i.loaded_from, 0x1001);
    }

    #[test]
    fn test_prepare_operands_two_custom_i() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1002, 0x20);
        cpu.writ_byte(0x1003, 0x40);
        let mut i = Instruction::decode_by(0xCC);

        load_operand(&mut cpu, &mut i, 0x1001);

        assert_eq!(cpu.pc(), 0xFEBE);
        assert_eq!(cpu.i, Some(Instruction::decode_by(0x08)));
        assert_eq!(i.operand, Some(0x4020));
        assert_eq!(i.amode_output, NotExecuted);
        assert_eq!(i.loaded_from, 0x1001);
    }

    #[test]
    fn test_stringify_disassembly() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x00);
        cpu.writ_byte(0x1001, 0xA9);
        cpu.writ_byte(0x1002, 0x10);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from("0x1000\tbrk\t\t| Imp\n");
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringify_disassembly_implied_and_immediate_am_instructions() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x00);
        cpu.writ_byte(0x1001, 0xA9);
        cpu.writ_byte(0x1002, 0x10);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 2);

        let expected_str = String::from(
            "\
                0x1000\tbrk\t\t| Imp\n\
                0x1001\tlda\t#0x10\t| Imm\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringidy_disassembly_zero_page_and_absolute() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x6D);
        cpu.writ_byte(0x1001, 0xAA);
        cpu.writ_byte(0x1002, 0xBB);
        cpu.writ_byte(0x1003, 0x65);
        cpu.writ_byte(0x1004, 0x30);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 5);

        let expected_str = String::from(
            "\
                0x1000\tadc\t0xbbaa\t| Abs\n\
                0x1003\tadc\t0x30\t| Zp0\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringidy_disassembly_zero_page_and_absolute_offset_x() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x7D);
        cpu.writ_byte(0x1001, 0xAA);
        cpu.writ_byte(0x1002, 0xBB);
        cpu.writ_byte(0x1003, 0x75);
        cpu.writ_byte(0x1004, 0x30);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 5);

        let expected_str = String::from(
            "\
                0x1000\tadc\t0xbbaa, X\t| Abx\n\
                0x1003\tadc\t0x30, X\t| Zpx\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringidy_disassembly_zero_page_and_absolute_offset_y() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0xB9);
        cpu.writ_byte(0x1001, 0xAA);
        cpu.writ_byte(0x1002, 0xBB);
        cpu.writ_byte(0x1003, 0xB6);
        cpu.writ_byte(0x1004, 0x30);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 5);

        let expected_str = String::from(
            "\
                0x1000\tlda\t0xbbaa, Y\t| Aby\n\
                0x1003\tldx\t0x30, Y\t| Zpy\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringidy_disassembly_indirect() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x6C);
        cpu.writ_byte(0x1001, 0xAA);
        cpu.writ_byte(0x1002, 0xBB);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from(
            "\
                0x1000\tjmp\t(0xbbaa)\t| Ind\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringidy_disassembly_relative() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x90);
        cpu.writ_byte(0x1001, 0x80);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from(
            "\
                0x1000\tbcc\t0x80\t| Rel\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringidy_disassembly_indirect_offset_y() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x91);
        cpu.writ_byte(0x1001, 0x10);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from(
            "\
                0x1000\tsta\t(0x10), Y\t| Iny\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringidy_disassembly_indirect_offset_x() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x81);
        cpu.writ_byte(0x1001, 0x10);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from(
            "\
                0x1000\tsta\t(0x10, X)\t| Inx\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }
}
