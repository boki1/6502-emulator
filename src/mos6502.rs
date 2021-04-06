use getset::{CopyGetters, Getters, MutGetters, Setters};
use std::cell::RefCell;
use std::rc::Rc;

pub type Address = u16;
pub type Word = u16;
pub type Opcode = u16;
pub type Byte = u8;

pub type AddressingModeFn = fn(&mut Cpu);
pub type InstructionFn = fn(&mut Cpu);

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

/// **Jump vectors**
/// In any of this exception cases - NMI, IRQ, BRK or a RESET, the PC should jump to the
/// concrete address, also called _vector_.
const NMI_VECTOR: Address = 0xfffa;
const RESET_VECTOR: Address = 0xfffc;
const IRQ_BRK_VECTOR: Address = 0xfffe;

#[derive(Getters, CopyGetters, Setters, MutGetters)]
pub struct Cpu {
    regset: RegisterSet,
    time: Timings,
    inter: InterruptHandling,
    bus_conn: Option<Rc<RefCell<dyn CommunicationInterface>>>,

    // Helper fields
    #[getset(get_copy = "pub", set = "pub", get_mut = "pub")]
    fetched: Byte,

    #[getset(get_copy = "pub", set = "pub", get_mut = "pub")]
    next_address: Address
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
                residual: 0,
            },
            inter: InterruptHandling {
                pending_irq: false,
                pending_nmi: false,
            },
            bus_conn: None,
            fetched: 0x00,
            next_address: 0x0000
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

    fn new_custompc(custom_prog_counter: Address) -> Self {
        Self {
            regset: RegisterSet::new_custompc(custom_prog_counter),
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

        let timing = Timings {
            residual: 8,
            elapsed: 0,
        };
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

const RAM_SIZE: usize = 0xffff;

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

///
/// **make_instr!** - Makes a new Instruction instance with
/// given field values
///
macro_rules! make_instr {
    ($p_amode: ident, $p_fun: ident, $p_time: expr, $p_mnemonic: literal, $p_opcode: expr) => {
        Instruction {
            amode: $p_amode,
            fun: $p_fun,
            time: $p_time,
            mnemonic: String::from($p_mnemonic),
            opcode: $p_opcode,
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

struct Instruction {
    amode: AddressingModeFn,
    fun: InstructionFn,
    time: u16,
    mnemonic: String,
    opcode: Opcode,
}

impl Instruction {
    ///
    /// **decode_by** - Match a given opcode value to its corresponding
    /// instruction.
    ///
    /// The table is filled according to [this](https://www.masswerk.at/6502/6502_instruction_set.html) resource.
    ///
    /// **NB:** Illegal opcodes are not supported as of now
    ///
    fn decode_by(opcode: Opcode) -> Instruction {
        use m6502_intruction_set::*;
        use m6502_addressing_modes::*;

        return match opcode {
            0x00 => make_instr!(implied_am, brk, 7, "brk", 0x00),
            0x01 => make_instr!(indirect_x_am, ora, 6, "ora", 0x01),
            0x05 => make_instr!(zeropage_am, ora, 3, "ora", 0x05),
            0x06 => make_instr!(zeropage_am, asl, 5, "asl", 0x06),
            0x08 => make_instr!(implied_am, php, 3, "php", 0x08),
            0x09 => make_instr!(immediate_am, ora, 2, "ora", 0x09),
            0x0A => make_instr!(implied_am, asl, 2, "asl", 0xA),
            0x0D => make_instr!(absolute_am, ora, 4, "ora", 0xD),
            0x0E => make_instr!(absolute_am, asl, 6, "asl", 0xE),

            0x10 => make_instr!(relative_am, bpl, 2, "bpl", 0x10),
            0x11 => make_instr!(indirect_y_am, ora, 5, "ora", 0x11),
            0x15 => make_instr!(zeropage_x_am, ora, 4, "ora", 0x15),
            0x16 => make_instr!(zeropage_x_am, asl, 6, "asl", 0x16),
            0x18 => make_instr!(implied_am, clc, 2, "clc", 0x18),
            0x19 => make_instr!(absolute_am, ora, 4, "ora", 0x19),
            0x1D => make_instr!(absolute_x_am, ora, 4, "ora", 0x1D),
            0x1E => make_instr!(absolute_x_am, asl, 7, "asl", 0x1E),

            0x20 => make_instr!(absolute_am, jsr, 6, "jsr", 0x20),
            0x21 => make_instr!(indirect_x_am, and, 6, "and", 0x21),
            0x24 => make_instr!(zeropage_am, bit, 3, "bit", 0x24),
            0x25 => make_instr!(zeropage_am, and, 3, "and", 0x25),
            0x26 => make_instr!(zeropage_am, rol, 5, "rol", 0x26),
            0x28 => make_instr!(implied_am, plp, 4, "plp", 0x28),
            0x29 => make_instr!(immediate_am, and, 2, "and", 0x29),
            0x2A => make_instr!(implied_am, rol, 2, "rol", 0x2A),
            0x2C => make_instr!(absolute_am, bit, 4, "bit", 0x2C),
            0x2D => make_instr!(absolute_am, and, 4, "and", 0x2D),
            0x2E => make_instr!(absolute_am, rol, 6, "rol", 0x2E),

            0x30 => make_instr!(relative_am, bmi, 2, "bmi", 0x30),
            0x31 => make_instr!(indirect_y_am, and, 5, "and", 0x31),
            0x35 => make_instr!(zeropage_x_am, and, 4, "and", 0x35),
            0x36 => make_instr!(zeropage_x_am, rol, 6, "rol", 0x36),
            0x38 => make_instr!(implied_am, sec, 2, "sec", 0x38),
            0x39 => make_instr!(absolute_y_am, and, 4, "and", 0x39),
            0x3D => make_instr!(absolute_x_am, and, 4, "and", 0x3D),
            0x3E => make_instr!(absolute_x_am, rol, 7, "rol", 0x3E),

            0x40 => make_instr!(implied_am, rti, 6, "rti", 0x40),
            0x41 => make_instr!(indirect_x_am, eor, 6, "eor", 0x41),
            0x45 => make_instr!(zeropage_am, eor, 3, "eor", 0x45),
            0x46 => make_instr!(zeropage_am, lsr, 5, "lsr", 0x46),
            0x48 => make_instr!(implied_am, pha, 3, "pha", 0x48),
            0x49 => make_instr!(immediate_am, eor, 2, "eor", 0x49),
            0x4A => make_instr!(implied_am, lsr, 2, "lsr", 0x4A),
            0x4C => make_instr!(absolute_am, jmp, 3, "jmp", 0x4C),
            0x4D => make_instr!(absolute_am, eor, 4, "eor", 0x4D),
            0x4E => make_instr!(absolute_am, lsr, 6, "lsr", 0x4E),

            0x50 => make_instr!(relative_am, bvc, 2, "bvc", 0x50),
            0x51 => make_instr!(indirect_y_am, eor, 5, "eor", 0x51),
            0x55 => make_instr!(zeropage_x_am, eor, 4, "eor", 0x55),
            0x56 => make_instr!(zeropage_x_am, lsr, 6, "lsr", 0x56),
            0x58 => make_instr!(implied_am, cli, 2, "cli", 0x58),
            0x59 => make_instr!(absolute_y_am, eor, 4, "eor", 0x59),
            0x5D => make_instr!(absolute_x_am, eor, 4, "eor", 0x5D),
            0x5E => make_instr!(absolute_x_am, lsr, 7, "lsr", 0x5E),

            0x60 => make_instr!(implied_am, rts, 6, "rts", 0x60),
            0x61 => make_instr!(indirect_x_am, adc, 6, "adc", 0x61),
            0x65 => make_instr!(zeropage_am, adc, 3, "adc", 0x65),
            0x66 => make_instr!(zeropage_am, ror, 5, "ror", 0x66),
            0x68 => make_instr!(implied_am, pla, 4, "pla", 0x68),
            0x69 => make_instr!(immediate_am, adc, 2, "adc", 0x69),
            0x6A => make_instr!(implied_am, ror, 2, "ror", 0x6A),
            0x6C => make_instr!(indirect_am, jmp, 5, "jmp", 0x6C),
            0x6D => make_instr!(absolute_am, adc, 4, "adc", 0x6D),
            0x6E => make_instr!(absolute_am, ror, 6, "ror", 0x6E),

            0x70 => make_instr!(relative_am, bvs, 2, "bvs", 0x70),
            0x71 => make_instr!(indirect_y_am, adc, 5, "adc", 0x71),
            0x75 => make_instr!(zeropage_x_am, adc, 4, "adc", 0x75),
            0x76 => make_instr!(zeropage_x_am, ror, 6, "ror", 0x76),
            0x78 => make_instr!(implied_am, sei, 2, "sei", 0x78),
            0x79 => make_instr!(absolute_y_am, adc, 4, "adc", 0x79),
            0x7D => make_instr!(absolute_x_am, adc, 4, "adc", 0x7D),
            0x7E => make_instr!(absolute_x_am, ror, 7, "ror", 0x7E),

            0x81 => make_instr!(indirect_x_am, sta, 6, "sta", 0x81),
            0x84 => make_instr!(zeropage_am, sty, 3, "sty", 0x84),
            0x85 => make_instr!(zeropage_am, sta, 3, "sta", 0x85),
            0x86 => make_instr!(zeropage_am, stx, 3, "stx", 0x86),
            0x88 => make_instr!(implied_am, dey, 2, "dey", 0x88),
            0x8A => make_instr!(implied_am, txa, 2, "txa", 0x8A),
            0x8C => make_instr!(absolute_am, sty, 4, "sty", 0x8C),
            0x8D => make_instr!(absolute_am, sta, 4, "sta", 0x8D),
            0x8E => make_instr!(absolute_am, stx, 4, "stx", 0x8E),

            0x90 => make_instr!(relative_am, bcc, 2, "bcc", 0x90),
            0x91 => make_instr!(indirect_y_am, sta, 6, "sta", 0x91),
            0x94 => make_instr!(zeropage_x_am, sty, 4, "sty", 0x94),
            0x95 => make_instr!(zeropage_x_am, sta, 4, "sta", 0x95),
            0x96 => make_instr!(zeropage_y_am, stx, 4, "stx", 0x96),
            0x98 => make_instr!(implied_am, tya, 2, "tya", 0x98),
            0x99 => make_instr!(absolute_y_am, sta, 5, "sta", 0x99),
            0x9A => make_instr!(implied_am, txs, 2, "txs", 0x9A),
            0x9D => make_instr!(absolute_x_am, sta, 5, "sta", 0x9D),

            0xA0 => make_instr!(immediate_am, ldy, 2, "ldy", 0xA0),
            0xA1 => make_instr!(indirect_x_am, lda, 6, "lda", 0xA1),
            0xA2 => make_instr!(immediate_am, ldx, 2, "ldx", 0xA2),
            0xA4 => make_instr!(zeropage_am, ldy, 3, "ldy", 0xA4),
            0xA5 => make_instr!(zeropage_am, lda, 3, "lda", 0xA5),
            0xA6 => make_instr!(zeropage_am, ldx, 3, "lda", 0xA6),
            0xA8 => make_instr!(implied_am, tay, 2, "tay", 0xA8),
            0xA9 => make_instr!(immediate_am, lda, 2, "lda", 0xA9),
            0xAA => make_instr!(implied_am, tax, 2, "tax", 0xAA),
            0xAC => make_instr!(absolute_am, ldy, 4, "ldy", 0xAC),
            0xAD => make_instr!(absolute_am, lda, 4, "lda", 0xAD),
            0xAE => make_instr!(absolute_am, ldx, 4, "ldx", 0xAE),

            0xB0 => make_instr!(relative_am, bcs, 2, "bcs", 0xB0),
            0xB1 => make_instr!(indirect_y_am, lda, 5, "lda", 0xB1),
            0xB4 => make_instr!(zeropage_x_am, ldy, 4, "ldy", 0xB4),
            0xB5 => make_instr!(zeropage_x_am, lda, 4, "lda", 0xB5),
            0xB6 => make_instr!(zeropage_y_am, ldx, 4, "ldx", 0xB6),
            0xB8 => make_instr!(implied_am, clv, 2, "clv", 0xB8),
            0xB9 => make_instr!(absolute_y_am, lda, 4, "lda", 0xB9),
            0xBA => make_instr!(implied_am, tsx, 2, "tsx", 0xBA),
            0xBC => make_instr!(absolute_x_am, ldy, 4, "ldy", 0xBC),
            0xBD => make_instr!(absolute_x_am, lda, 4, "lda", 0xBD),
            0xBE => make_instr!(absolute_y_am, ldx, 4, "ldx", 0xBE),

            0xC0 => make_instr!(immediate_am, cpy, 2, "cpy", 0xC0),
            0xC1 => make_instr!(indirect_x_am, cmp, 6, "cmp", 0xC1),
            0xC4 => make_instr!(zeropage_am, cpy, 3, "cpy", 0xC4),
            0xC5 => make_instr!(zeropage_am, cmp, 3, "cmp", 0xC5),
            0xC6 => make_instr!(zeropage_am, dec, 5, "dec", 0xC6),
            0xC8 => make_instr!(implied_am, iny, 2, "iny", 0xC8),
            0xC9 => make_instr!(immediate_am, cmp, 2, "cmp", 0xC9),
            0xCA => make_instr!(implied_am, dex, 2, "dex", 0xCA),
            0xCC => make_instr!(absolute_am, cpy, 4, "cpy", 0xCC),
            0xCD => make_instr!(absolute_am, cmp, 4, "cmp", 0xCD),
            0xCE => make_instr!(absolute_am, dec, 6, "dec", 0xCE),

            0xD0 => make_instr!(relative_am, bne, 2, "bne", 0xD0),
            0xD1 => make_instr!(indirect_y_am, cmp, 5, "cmp", 0xD1),
            0xD5 => make_instr!(zeropage_x_am, cmp, 4, "cmp", 0xD5),
            0xD6 => make_instr!(zeropage_x_am, dec, 6, "dec", 0xD6),
            0xD8 => make_instr!(implied_am, cld, 2, "cld", 0xD8),
            0xD9 => make_instr!(absolute_y_am, cmp, 4, "cmp", 0xD9),
            0xDD => make_instr!(absolute_x_am, cmp, 4, "cmp", 0xDD),
            0xDE => make_instr!(absolute_x_am, dec, 7, "dec", 0xDE),

            0xE0 => make_instr!(immediate_am, cpx, 2, "cpx", 0xE0),
            0xE1 => make_instr!(indirect_x_am, sbc, 6, "sbc", 0xE1),
            0xE4 => make_instr!(zeropage_am, cpx, 3, "cpx", 0xE4),
            0xE5 => make_instr!(zeropage_am, sbc, 3, "sbc", 0xE5),
            0xE6 => make_instr!(zeropage_am, inc, 6, "inc", 0xE6),
            0xE8 => make_instr!(implied_am, inx, 2, "inx", 0xE8),
            0xE9 => make_instr!(immediate_am, sbc, 2, "sbc", 0xE9),
            0xEA => make_instr!(implied_am, nop, 2, "nop", 0xEA),
            0xEC => make_instr!(absolute_am, cpx, 4, "cpx", 0xEC),
            0xED => make_instr!(absolute_am, sbc, 4, "sbc", 0xED),
            0xEE => make_instr!(absolute_am, inc, 6, "inc", 0xEE),

            0xF0 => make_instr!(relative_am, beq, 2, "beq", 0xF0),
            0xF1 => make_instr!(indirect_y_am, sbc, 5, "sbc", 0xF1),
            0xF5 => make_instr!(zeropage_x_am, sbc, 4, "sbc", 0xF5),
            0xF6 => make_instr!(zeropage_x_am, inc, 6, "inc", 0xF6),
            0xF8 => make_instr!(implied_am, sed, 2, "sed", 0xF8),
            0xF9 => make_instr!(absolute_y_am, sbc, 4, "sbc", 0xF9),
            0xFD => make_instr!(absolute_x_am, sbc, 4, "sbc", 0xFD),
            0xFE => make_instr!(absolute_x_am, inc, 7, "inc", 0xFE),

            _ => make_illegal!(),
        };
    }
}

///
/// Instruction set
/// Legal MOS 6502 instructions
///
mod m6502_intruction_set {
    use super::Cpu;

    pub fn adc(cpu: &mut Cpu) {}
    pub fn and(cpu: &mut Cpu) {}
    pub fn asl(cpu: &mut Cpu) {}
    pub fn bcc(cpu: &mut Cpu) {}
    pub fn bcs(cpu: &mut Cpu) {}
    pub fn beq(cpu: &mut Cpu) {}
    pub fn bit(cpu: &mut Cpu) {}
    pub fn bmi(cpu: &mut Cpu) {}
    pub fn bne(cpu: &mut Cpu) {}
    pub fn bpl(cpu: &mut Cpu) {}
    pub fn brk(cpu: &mut Cpu) {}
    pub fn bvc(cpu: &mut Cpu) {}
    pub fn bvs(cpu: &mut Cpu) {}
    pub fn clc(cpu: &mut Cpu) {}
    pub fn cld(cpu: &mut Cpu) {}
    pub fn cli(cpu: &mut Cpu) {}
    pub fn clv(cpu: &mut Cpu) {}
    pub fn cmp(cpu: &mut Cpu) {}
    pub fn cpx(cpu: &mut Cpu) {}
    pub fn cpy(cpu: &mut Cpu) {}
    pub fn dec(cpu: &mut Cpu) {}
    pub fn dex(cpu: &mut Cpu) {}
    pub fn dey(cpu: &mut Cpu) {}
    pub fn eor(cpu: &mut Cpu) {}
    pub fn inc(cpu: &mut Cpu) {}
    pub fn inx(cpu: &mut Cpu) {}
    pub fn iny(cpu: &mut Cpu) {}
    pub fn jmp(cpu: &mut Cpu) {}
    pub fn jsr(cpu: &mut Cpu) {}
    pub fn lda(cpu: &mut Cpu) {}
    pub fn ldx(cpu: &mut Cpu) {}
    pub fn ldy(cpu: &mut Cpu) {}
    pub fn lsr(cpu: &mut Cpu) {}
    pub fn nop(cpu: &mut Cpu) {}
    pub fn ora(cpu: &mut Cpu) {}
    pub fn pha(cpu: &mut Cpu) {}
    pub fn php(cpu: &mut Cpu) {}
    pub fn pla(cpu: &mut Cpu) {}
    pub fn plp(cpu: &mut Cpu) {}
    pub fn rol(cpu: &mut Cpu) {}
    pub fn ror(cpu: &mut Cpu) {}
    pub fn rti(cpu: &mut Cpu) {}
    pub fn rts(cpu: &mut Cpu) {}
    pub fn sbc(cpu: &mut Cpu) {}
    pub fn sec(cpu: &mut Cpu) {}
    pub fn sed(cpu: &mut Cpu) {}
    pub fn sei(cpu: &mut Cpu) {}
    pub fn sta(cpu: &mut Cpu) {}
    pub fn stx(cpu: &mut Cpu) {}
    pub fn sty(cpu: &mut Cpu) {}
    pub fn tax(cpu: &mut Cpu) {}
    pub fn tay(cpu: &mut Cpu) {}
    pub fn tsx(cpu: &mut Cpu) {}
    pub fn txa(cpu: &mut Cpu) {}
    pub fn txs(cpu: &mut Cpu) {}
    pub fn tya(cpu: &mut Cpu) {}
}

///
/// Addressing modes
/// The 6502 cpu support different kinds of _addressing mode_/
///
///
mod m6502_addressing_modes {

    use super::{Cpu, Byte, Address, Word, MainBus};
    use std::cell::RefCell;
    use std::rc::Rc;

    ///
    /// **Implied** 
    /// This addressing mode is the simplest one, because it is
    /// use only with instructions which do not need any additional
    /// context -- it is _implied_.
    /// 
    pub fn implied_am(cpu: &mut Cpu) {
        let accumulator = cpu.regset().accumulator();
        cpu.set_fetched(accumulator as u8);
    }

    ///
    /// **Immediate**
    /// This addressing mode is used when the next byte to be
    /// used a value.
    /// 
    pub fn immediate_am(cpu: &mut Cpu) {
        let next_address = cpu.regset().prog_counter() + 1;
        cpu.set_next_address(next_address);
        let fetched = cpu.read_byte(next_address);
        cpu.set_fetched(fetched);
    }

    ///
    /// **Zero page**
    /// 
    /// In order to utilize the system capabilities more efficiently
    /// it is possible to address _absolutely_ the first page of
    /// memory _faster_ by providing a 8-bit number instead of 16-bit
    /// for address value (6502 addresses are 16-bit).
    /// 
    pub fn zeropage_am(cpu: &mut Cpu) {
        let next_address = Address::from(zeropage_next_address(cpu));
        let fetched = cpu.read_byte(next_address);
        cpu.set_next_address(next_address);
        cpu.set_fetched(fetched);
    }

    ///
    /// **Zero page with X offset**
    /// 
    /// Same as Zero page but indexes using the value inside
    /// the X-index register. If the address is bigger than
    /// the maximum of zero page, it wrapps around.
    /// 
    pub fn zeropage_x_am(cpu: &mut Cpu) {
        let offset = cpu.regset().x_index() as Byte;
        let next_address = Address::from(zeropage_next_address(cpu).wrapping_add(offset));
        let fetched = cpu.read_byte(next_address);
        cpu.set_next_address(next_address);
        cpu.set_fetched(fetched);
    } 

    ///
    /// **Zero page with Y offset**
    /// 
    /// Same as Zero page with X offset but using the value
    /// inside the Y-index register instead of the X-index.
    /// 
    pub fn zeropage_y_am(cpu: &mut Cpu) {
        let offset = cpu.regset().y_index() as Byte;
        let next_address = Address::from(zeropage_next_address(cpu).wrapping_add(offset));
        let fetched = cpu.read_byte(next_address);
        cpu.set_next_address(next_address);
        cpu.set_fetched(fetched);
    }

    /// A helper function, used with the zeropage 
    /// addressing modes
    fn zeropage_next_address(cpu: &mut Cpu) -> Byte {
        let progcounter = cpu.regset().prog_counter();
        *cpu.regset_mut().prog_counter_mut() += 1;
        cpu.read_byte(progcounter)
    }

    ///
    /// **Absolute**
    /// Specifies the memory location explicitly in the two bytes
    /// following the opcode. In order to fetch the value which
    /// is going to be used, the full 16-bit address has to
    /// acquired and then read from.
    /// 
    pub fn absolute_am(cpu: &mut Cpu) {
        let next_address = absolute_next_address(cpu);
        let value = cpu.read_byte(next_address);
        cpu.set_next_address(next_address);
        cpu.set_fetched(value);
    }

    /// 
    /// **Absolute with X offset**
    /// 
    /// Analogical to the absolute addressing mode but
    /// uses the value of the X-index register as an offset
    /// before actually reading the value.
    pub fn absolute_x_am(cpu: &mut Cpu) {
        let x_index = cpu.regset().x_index();
        let next_address = absolute_next_address(cpu);
        let offset_address = next_address + Address::from(x_index);
        let value = cpu.read_byte(next_address);
        cpu.set_next_address(next_address);
        cpu.set_fetched(value);

        if next_address & 0xFF00 != offset_address & 0xFF00 {
            mark_extra_clockcycle(cpu);
        }
    }

    ///
    /// **Absolute with Y offset**
    /// 
    /// Same as the absolute with X offset but uses the
    /// Y-index register instead.
    /// 
    pub fn absolute_y_am(cpu: &mut Cpu) {
        let y_index = cpu.regset().y_index();
        let next_address = absolute_next_address(cpu);
        let offset_address = next_address + Address::from(y_index);
        let value = cpu.read_byte(offset_address);
        cpu.set_next_address(offset_address);
        cpu.set_fetched(value);

        if next_address & 0xFF00 != offset_address & 0xFF00 {
            mark_extra_clockcycle(cpu);
        }
    }

    /// A helper function, used with the absolute
    /// addressing modes
    fn absolute_next_address(cpu: &mut Cpu) -> Address {
        let mut pc = cpu.regset().prog_counter();
        let lo = cpu.read_byte(pc);    pc += 1;
        let hi = cpu.read_byte(pc);    pc += 1;
        let next_address = Address::from_le_bytes([lo, hi]);
        *cpu.regset_mut().prog_counter_mut() = pc;
        next_address
    }

    ///
    /// **Relative**
    /// 
    /// The supplied byte is interpreted as a _signed offset_
    /// This offset is then added to the program counter.
    /// 
    pub fn relative_am(cpu: &mut Cpu) {
        let mut pc = cpu.regset().prog_counter();
        let rel = cpu.read_byte(pc);
        pc = pc.wrapping_add(signedbyte_to_word(rel)); 
        cpu.set_next_address(pc);

        let value = cpu.read_byte(pc); pc += 1;
        cpu.set_fetched(value);
        *cpu.regset_mut().prog_counter_mut() = pc;
    }

    fn signedbyte_to_word(p_num: Byte) -> Word {
        let num = Word::from(p_num);
        return if num & 0x80 != 0 {
            num | 0xFF00
        } else {
            num
        };
    } 

    ///
    /// **Indirect**
    /// 
    /// The supplied 16-bit address is set as a value for the
    /// program counter.
    /// 
    /// **NB:** This operation has a hardware bug when
    /// a page boundary is crossed. 
    pub fn indirect_am(cpu: &mut Cpu) {
        let mut pc = cpu.regset().prog_counter();

        let ptr_lo = cpu.read_byte(pc); pc += 1;
        let ptr_hi = cpu.read_byte(pc); pc += 1;
        let ptr = Address::from_le_bytes([ptr_lo, ptr_hi]);
        *cpu.regset_mut().prog_counter_mut() = pc;

        let next_address_lo = cpu.read_byte(ptr);
        let next_address_hi = cpu.read_byte(if ptr_lo == 0x00FF { ptr & 0xFF00 } else { ptr + 1 });
        let next_address = Address::from_le_bytes([next_address_lo, next_address_hi]);

        let value = cpu.read_byte(next_address);
        cpu.set_next_address(next_address);
        cpu.set_fetched(value);
    }

    ///
    /// **Indirect with X-index offset**
    /// 
    /// A 8-bit address is suplied. It is then offset
    /// by the value of the X-index register to a
    /// location in the zero page. Then the actual
    /// address is read.
    pub fn indirect_x_am(cpu: &mut Cpu) {
        let mut pc = cpu.regset().prog_counter();
        let base = cpu.read_byte(pc);    pc += 1;

        let x_index = cpu.regset().x_index();
        let offset = Address::from(base.wrapping_add(x_index));

        let lo = cpu.read_byte(offset);
        let hi = cpu.read_byte(offset + 1 & 0x00FF);

        let next_address = Address::from_le_bytes([lo, hi]);
        cpu.set_next_address(next_address);

        let value = cpu.read_byte(next_address);
        cpu.set_fetched(value);
        *cpu.regset_mut().prog_counter_mut() = pc;
    }

    pub fn indirect_y_am(cpu: &mut Cpu)  {
        let mut pc = cpu.regset().prog_counter();
        let base = Address::from(cpu.read_byte(pc));
        pc += 1;
        *cpu.regset_mut().prog_counter_mut() = pc;

        let lo = cpu.read_byte(base);
        let hi = cpu.read_byte(base + 1 & 0x00FF);
        let next_address = Address::from_le_bytes([lo, hi]);

        let y_index = cpu.regset().y_index();
        let offset_address = next_address.wrapping_add(y_index as Address);
        cpu.set_next_address(offset_address);

        if next_address & 0xFF00 != offset_address & 0xFF00 {
            mark_extra_clockcycle(cpu);
        }

        let value = cpu.read_byte(offset_address);
        cpu.set_fetched(value);
    }

    fn mark_extra_clockcycle(cpu: &mut Cpu) {
        *cpu.time_mut().residual_mut() += 1;
    }

    #[cfg(test)]
    mod test{
        use super::*;

        #[test]
        fn test__implied_am() {
            let mut cpu = Cpu::new_custompc(0x0000);
            cpu.regset_mut().set_accumulator(0x1A);

            implied_am(&mut cpu);

            let fetched = cpu.fetched();
            let accumulator = cpu.regset().accumulator();
            let prog_counter = cpu.regset().prog_counter();

            assert_eq!(fetched, accumulator);
            assert_eq!(prog_counter, 0x0000);
        }

        #[test]
        fn test__immediate_am() {
            let mut cpu = Cpu::new_custompc(0x00FA - 1);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            let expected = 0xA;
            cpu.writ_byte(0xFA, expected);

            immediate_am(&mut cpu);

            let fetched = cpu.fetched();
            
            assert_eq!(fetched, expected);
        }

        #[test]
        fn test__zeropage_am() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            cpu.writ_byte(0x00, 0x35);
            cpu.writ_byte(0x35, 0x10);

            zeropage_am(&mut cpu);
            let fetched = cpu.fetched();

            assert_eq!(fetched, 0x10);
        }

        #[test]
        fn test__zeropage_x_offset_am() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
            *cpu.regset_mut().x_index_mut() = 3;

            cpu.writ_byte(0x00, 0x35);
            cpu.writ_byte(0x35 + 3, 0x10);

            zeropage_x_am(&mut cpu);
            let fetched = cpu.fetched();

            assert_eq!(fetched, 0x10);
        }

        #[test]
        fn test__zeropage_offset_wrapping_am() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
            *cpu.regset_mut().y_index_mut() = 0xff;

            cpu.writ_byte(0x00, 0x35);
            cpu.writ_byte(0x35 - 1, 0x10);

            zeropage_y_am(&mut cpu);
            let fetched = cpu.fetched();

            assert_eq!(fetched, 0x10);
        }

        #[test]
        fn test__absolute_am() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            cpu.writ_byte(0x00, 0x10);
            cpu.writ_byte(0x01, 0x02);
            cpu.writ_byte(0x0210, 0x10);

            absolute_am(&mut cpu);
            let fetched = cpu.fetched();

            assert_eq!(fetched, 0x10);
        }

        #[test]
        fn test__absolute_offset_am() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            *cpu.regset_mut().y_index_mut() = 0xA;

            cpu.writ_byte(0x00, 0x10);
            cpu.writ_byte(0x01, 0x02);
            cpu.writ_byte(0x0210 + 0xA, 0x10);

            absolute_y_am(&mut cpu);
            let fetched = cpu.fetched();

            assert_eq!(fetched, 0x10);
        }

        #[test]
        fn test__relative_am_w_positive_number() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            cpu.writ_byte(0x00, 0x10);
            cpu.writ_byte(0x10, 0x10);

            relative_am(&mut cpu);
            let fetched = cpu.fetched();

            assert_eq!(fetched, 0x10);
        }

        #[test]
        fn test__relative_am_w_negative_number() {
            let mut cpu = Cpu::new_custompc(0x90);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            cpu.writ_byte(0x90, 0x80);
            cpu.writ_byte(0x10, 0x10);

            relative_am(&mut cpu);
            let fetched = cpu.fetched();

            assert_eq!(fetched, 0x10);
        }

        #[test]
        fn test__indirect_am_page_cross() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            cpu.writ_byte(0x00, 0x11);
            cpu.writ_byte(0x01, 0x10);

            cpu.writ_byte(0x1011, 0x01);
            cpu.writ_byte(0x1012, 0xFF);

            indirect_am(&mut cpu);

            let next_address = cpu.next_address();
            assert_eq!(next_address, 0xFF01);
        }

        #[test]
        fn test__indirect_am() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            cpu.writ_byte(0x00, 0xFF);
            cpu.writ_byte(0x01, 0x10);

            cpu.writ_byte(0x10FF, 0x01);
            cpu.writ_byte(0x1000, 0xA7);

            indirect_am(&mut cpu);

            let next_address = cpu.next_address();
            assert_eq!(next_address, 0xA701);
        }

        #[test]
        fn test__indirect_xoffset_am_1() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            *cpu.regset_mut().x_index_mut() = 0x04;

            cpu.writ_byte(0x00, 0x20);

            cpu.writ_byte(0x24, 0x74);
            cpu.writ_byte(0x25, 0x20);
            cpu.writ_byte(0x2074, 0x10);

            indirect_x_am(&mut cpu);

            let next_address = cpu.next_address();
            assert_eq!(next_address, 0x2074);

            let fetched = cpu.fetched();
            assert_eq!(fetched, 0x10);
        }

        #[test]
        fn test__indirect_xoffset_am_2() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            *cpu.regset_mut().x_index_mut() = 0x10;

            cpu.writ_byte(0x00, 0x25);

            cpu.writ_byte(0x35, 0x01);
            cpu.writ_byte(0x36, 0xA7);

            cpu.writ_byte(0xA701, 0x19);

            indirect_x_am(&mut cpu);

            let next_address = cpu.next_address();
            assert_eq!(next_address, 0xA701);

            let fetched = cpu.fetched();
            assert_eq!(fetched, 0x19);
        }

        #[test]
        fn test__indirect_xoffset_am_3() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            *cpu.regset_mut().x_index_mut() = 0x01;

            cpu.writ_byte(0x0000, 0xFE);
            cpu.writ_byte(0x00FF, 0x01);

            cpu.writ_byte(0xFE01, 0x17);

            indirect_x_am(&mut cpu);

            let next_address = cpu.next_address();
            assert_eq!(next_address, 0xFE01);

            let fetched = cpu.fetched();
            assert_eq!(fetched, 0x17);
        }

        #[test]
        fn test__indirect_yoffset_am_1() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            *cpu.regset_mut().y_index_mut() = 0x10;

            cpu.writ_byte(0x0000, 0x25);
            cpu.writ_byte(0x0025, 0xFF);
            cpu.writ_byte(0x0026, 0xA7);

            cpu.writ_byte(0xA80F, 0x34);

            indirect_y_am(&mut cpu);

            let next_address = cpu.next_address();
            assert_eq!(next_address, 0xA80F);

            let fetched = cpu.fetched();
            assert_eq!(fetched, 0x34);

            let marked_extra_cycle = cpu.time.residual == 1;
            assert_eq!(marked_extra_cycle, true);
        }

        #[test]
        fn test__indirect_yoffset_am_2() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            *cpu.regset_mut().y_index_mut() = 0x10;

            cpu.writ_byte(0x0000, 0x25);
            cpu.writ_byte(0x0025, 0x01);
            cpu.writ_byte(0x0026, 0xA7);

            cpu.writ_byte(0xA711, 0x34);

            indirect_y_am(&mut cpu);

            let next_address = cpu.next_address();
            assert_eq!(next_address, 0xA711);

            let fetched = cpu.fetched();
            assert_eq!(fetched, 0x34);

            let marked_extra_cycle = cpu.time.residual == 1;
            assert_eq!(marked_extra_cycle, false);
        }

        #[test]
        fn test__indirect_yoffset_am_3() {
            let mut cpu = Cpu::new_custompc(0x00);
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));

            *cpu.regset_mut().y_index_mut() = 0x10;

            cpu.writ_byte(0x0000, 0x86);
            cpu.writ_byte(0x0086, 0x28);
            cpu.writ_byte(0x0087, 0x40);

            cpu.writ_byte(0x4038, 0x37);

            indirect_y_am(&mut cpu);

            let next_address = cpu.next_address();
            assert_eq!(next_address, 0x4038);

            let fetched = cpu.fetched();
            assert_eq!(fetched, 0x37);

            let marked_extra_cycle = cpu.time.residual == 1;
            assert_eq!(marked_extra_cycle, false);
        }
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

#[test]
fn test__decode_by_correct() {
    let cpu = Cpu::new_connected(Some(Rc::new(RefCell::new(MainBus::new()))));
    let i = Instruction::decode_by(0x10);

    assert_eq!(i.mnemonic, "bpl".to_string());
    assert_eq!(i.opcode, 0x10);
    assert_eq!(i.time, 2);
}
