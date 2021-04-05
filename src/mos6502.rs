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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RegisterSet {
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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Timings {
    elapsed: u64,
    residual: u8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct InterruptHandling {
    pending_nmi: bool,
    pending_irq: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cpu {
    regset: RegisterSet,
    time: Timings,
    inter: InterruptHandling,
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
        }
    }
}

#[test]
fn create_cpu() {
    let _ = Cpu::new();
    assert!(true);
}
