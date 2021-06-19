use crate::mos6502::{
    Address, AddressingMode::*, AddressingOutput::*, Byte, Cpu, CpuError, RegisterSet, Word,
    BRK_VECTOR,
};

///
/// Instruction set
/// Legal MOS 6502 instructions
///

fn verify_and_fetch(cpu: &mut Cpu) -> Result<Word, CpuError> {
    let i = cpu.i();
    if let Some(i) = i {
        return match i.amode_output() {
            Fetched { value, address: _ } => Ok(Word::from(value)),
            ValueOnly(value) => Ok(Word::from(value)),
            AbsoluteAddress(address) => Ok(address),
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
pub(crate) fn do_branch(cpu: &mut Cpu, next_address: Address) {
    let prog_counter = cpu.regset().prog_counter();
    let cycles_inc = if page_of(next_address) == page_of(prog_counter) {
        1
    } else {
        2
    };

    *cpu.time_mut().residual_mut() += cycles_inc;
    cpu.regset_mut().set_prog_counter(next_address);
}

pub(crate) fn do_compare(regs: &mut RegisterSet, lhs: Byte, rhs: Byte) {
    let diff = lhs.wrapping_sub(rhs);

    regs.set_carry(lhs >= rhs);
    regs.set_zero(lhs == rhs);
    regs.set_negative(diff & 0x80 > 0);
}

/// Add Memory to Accumulator with Carry
///
/// `A = A + M + C`
///
/// **Negative:** If the result is negative\
/// **Carry:** If the result is more than 255\
/// **Zero:** If the result is 0\
/// **Overflow:** If the result has made a _sign_ overflowed
///
pub fn adc(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)?;

    let regs = cpu.regset_mut();
    let accumulator_u16 = u16::from(regs.accumulator());
    let tmp: u16 = accumulator_u16 + fetched + u16::from(regs.carry());
    let accumulator = (tmp & 0x00FF) as u8;

    regs.set_carry(tmp > 0xFF);
    regs.set_zero(accumulator == 0);
    regs.set_negative((tmp & 0x80) > 0);
    regs.set_overflowed((!(accumulator_u16 ^ fetched) & (accumulator_u16 ^ tmp) & 0x80) > 0);

    regs.set_accumulator(accumulator);

    Ok(())
}

/// Perform a bitwise "AND" with Memory with Accumulator
///
/// **Zero:** If the resulting accumulator value is 0\
/// **Negative:** If the resulting accumulator value is a negative number
pub fn and(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;

    let a = cpu.regset().accumulator() & fetched;
    let regs = cpu.regset_mut();

    regs.set_accumulator(a);

    regs.set_zero(a == 0x00);
    regs.set_negative(a & 0x80 > 0);

    Ok(())
}

/// Shift left once (Memory or Accumulator)
///
/// **Zero:** If the resulting value is 0\
/// **Negative:** If the resulting value is a negative number\
/// **Carry:** The highest bit of the value
pub fn asl(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched_u16 = verify_and_fetch(cpu)?;
    let fetched_shifted_u16 = fetched_u16 << 1;
    let val = fetched_u16 as u8;
    let val_shifted = (fetched_shifted_u16 & 0x00FF) as u8;

    let ii = cpu.i();
    if let Some(i) = ii {
        if i.amode() == Imp {
            let _ = cpu.regset_mut().set_accumulator(val_shifted);
        } else if let Fetched { value: _, address } = i.amode_output() {
            cpu.writ_byte(address, val_shifted);
        } else {
            return Err(CpuError::BadAddressing);
        }
    }

    let regs = cpu.regset_mut();
    regs.set_carry((fetched_shifted_u16 & 0xFF00) > 0);
    regs.set_zero(val_shifted == 0);
    regs.set_negative((fetched_shifted_u16 & 0x80) > 0);

    Ok(())
}

/// Branch on carry clear
pub fn bcc(cpu: &mut Cpu) -> Result<(), CpuError> {
    let next_address = verify_and_fetch(cpu)?;
    let regs = cpu.regset();

    if !regs.carry() {
        do_branch(cpu, next_address);
    }

    Ok(())
}

/// Branch on carry set
pub fn bcs(cpu: &mut Cpu) -> Result<(), CpuError> {
    let next_address = verify_and_fetch(cpu)?;
    let regs = cpu.regset();

    if regs.carry() {
        do_branch(cpu, next_address);
    }

    Ok(())
}

/// Branch on equal
pub fn beq(cpu: &mut Cpu) -> Result<(), CpuError> {
    let next_address = verify_and_fetch(cpu)?;
    let regs = cpu.regset();

    if regs.zero() {
        do_branch(cpu, next_address);
    }

    Ok(())
}

/// Test Bits in Memory with Accumulator
///
/// **Zero:** Set to the result of logical "AND" between the accumulator and the memory value\
/// **Negative:** Set to 7th bit of the memory value \
/// **Overflowed:** Set to the 6th bit of the memory value
pub fn bit(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)?;

    let regs = cpu.regset_mut();
    let a = regs.accumulator();
    regs.set_zero(u16::from(a) & fetched == 0);
    regs.set_negative(fetched & (1 << 7) > 0);
    regs.set_overflowed(fetched & (1 << 6) > 0);

    Ok(())
}

/// Branch on negative result
pub fn bmi(cpu: &mut Cpu) -> Result<(), CpuError> {
    let next_address = verify_and_fetch(cpu)?;
    let regs = cpu.regset();

    if regs.negative() {
        do_branch(cpu, next_address);
    }

    Ok(())
}

/// Branch on not-equal
pub fn bne(cpu: &mut Cpu) -> Result<(), CpuError> {
    let next_address = verify_and_fetch(cpu)?;
    let regs = cpu.regset();

    if !regs.zero() {
        do_branch(cpu, next_address);
    }

    Ok(())
}

/// Branch on positive result
pub fn bpl(cpu: &mut Cpu) -> Result<(), CpuError> {
    let next_address = verify_and_fetch(cpu)?;
    let regs = cpu.regset();

    if !regs.negative() {
        do_branch(cpu, next_address);
    }

    Ok(())
}

/// Force break -- Software Interrup
///
/// The value of the **Interrupt disabled** flag has to be 0 in order for this to execute
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

/// Branch on overflow clear
pub fn bvc(cpu: &mut Cpu) -> Result<(), CpuError> {
    let next_address = verify_and_fetch(cpu)?;
    let regs = cpu.regset();

    if !regs.overflowed() {
        do_branch(cpu, next_address);
    }

    Ok(())
}

/// Branch on overflow set
pub fn bvs(cpu: &mut Cpu) -> Result<(), CpuError> {
    let next_address = verify_and_fetch(cpu)?;
    let regs = cpu.regset();

    if regs.overflowed() {
        do_branch(cpu, next_address);
    }

    Ok(())
}

/// Clear carry flag
pub fn clc(cpu: &mut Cpu) -> Result<(), CpuError> {
    cpu.regset_mut().set_carry(false);
    Ok(())
}

/// Clear decimal mode flag
pub fn cld(cpu: &mut Cpu) -> Result<(), CpuError> {
    cpu.regset_mut().set_decimal_mode(false);
    Ok(())
}

/// Clear interrupt disabled flag
pub fn cli(cpu: &mut Cpu) -> Result<(), CpuError> {
    cpu.regset_mut().set_irq_disabled(false);

    Ok(())
}

/// Clear overflow flag
pub fn clv(cpu: &mut Cpu) -> Result<(), CpuError> {
    cpu.regset_mut().set_overflowed(false);

    Ok(())
}

/// Compare memory with accumulator
///
/// **Zero:** If the values are equal \
/// **Negative:** If the accumulator is smaller \
/// **Carry:** If the accumulator is less or equal
pub fn cmp(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;
    let regs: &mut RegisterSet = cpu.regset_mut();

    do_compare(regs, regs.accumulator(), fetched);

    Ok(())
}

///
/// Compare memory with X index
///
/// **Zero:** If the values are equal \
/// **Negative:** If the X index is smaller \
/// **Carry:** If the X index is less or equal
pub fn cpx(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;
    let regs: &mut RegisterSet = cpu.regset_mut();

    do_compare(regs, regs.x_index(), fetched);

    Ok(())
}

///
/// Compare memory with Y index
///
/// **Zero:** If the values are equal \
/// **Negative:** If the Y index is smaller \
/// **Carry:** If the Y index is less or equal
pub fn cpy(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;
    let regs = cpu.regset_mut();

    do_compare(regs, regs.y_index(), fetched);

    Ok(())
}

///
/// Decrement memory value by one
///
/// **Zero:** If the resuling value is equal to 0 \
/// **Negative:** If the resulting value is negative
pub fn dec(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = (verify_and_fetch(cpu)? as u8).wrapping_sub(1);
    let amode_output = cpu.i().unwrap().amode_output();

    let address = match amode_output {
        Fetched { value: _, address } => address,
        AbsoluteAddress(address) => address,
        _ => return Err(CpuError::BadAddressing),
    };

    cpu.writ_byte(address, fetched);
    let regs = cpu.regset_mut();
    regs.set_zero(fetched == 0);
    regs.set_negative(fetched & 0x80 > 0);

    Ok(())
}

///
/// Decrement X index by one
///
/// **Zero:** If the resuling value is equal to 0 \
/// **Negative:** If the resulting value is negative
pub fn dex(cpu: &mut Cpu) -> Result<(), CpuError> {
    let x = cpu.regset().x_index().wrapping_sub(1);
    let regs = cpu.regset_mut();

    regs.set_zero(x == 0);
    regs.set_negative(x & 0x80 > 0);

    regs.set_x_index(x);

    Ok(())
}

///
/// Decrement Y index by one
///
/// **Zero:** If the resuling value is equal to 0 \
/// **Negative:** If the resulting value is negative
pub fn dey(cpu: &mut Cpu) -> Result<(), CpuError> {
    let y = cpu.regset().y_index().wrapping_sub(1);
    let regs = cpu.regset_mut();

    regs.set_zero(y == 0);
    regs.set_negative(y & 0x80 > 0);

    regs.set_x_index(y);

    Ok(())
}

///
/// Exclusive-or between a memory value and the accumulator
///
/// **Zero:** If the resulting value is equal to 0 \
/// **Negative:** If the resulting value is negative
pub fn eor(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;
    let a = cpu.regset().accumulator() ^ fetched;
    let regs = cpu.regset_mut();

    regs.set_zero(a == 0);
    regs.set_negative(a & 0x80 > 0);

    regs.set_accumulator(a);

    Ok(())
}

///
/// Increment memory value by one
///
/// **Zero:** If the resuling value is equal to 0 \
/// **Negative:** If the resulting value is negative
pub fn inc(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = (verify_and_fetch(cpu)? as u8).wrapping_add(1);
    let amode_output = cpu.i().unwrap().amode_output();

    let address = match amode_output {
        Fetched { value: _, address } => address,
        AbsoluteAddress(address) => address,
        _ => return Err(CpuError::BadAddressing),
    };

    cpu.writ_byte(address, fetched);
    let regs = cpu.regset_mut();
    regs.set_zero(fetched == 0);
    regs.set_negative(fetched & 0x80 > 0);

    Ok(())
}

///
/// Increment X index by one
///
/// **Zero:** If the resuling value is equal to 0 \
/// **Negative:** If the resulting value is negative
pub fn inx(cpu: &mut Cpu) -> Result<(), CpuError> {
    let x = cpu.regset().x_index().wrapping_add(1);
    let regs = cpu.regset_mut();

    regs.set_zero(x == 0);
    regs.set_negative(x & 0x80 > 0);

    regs.set_x_index(x);

    Ok(())
}

///
/// Increment Y index by one
///
/// **Zero:** If the resuling value is equal to 0 \
/// **Negative:** If the resulting value is negative
pub fn iny(cpu: &mut Cpu) -> Result<(), CpuError> {
    let y = cpu.regset().y_index().wrapping_add(1);
    let regs = cpu.regset_mut();

    regs.set_zero(y == 0);
    regs.set_negative(y & 0x80 > 0);

    regs.set_y_index(y);

    Ok(())
}

///
/// Jump to a new location
///
pub fn jmp(cpu: &mut Cpu) -> Result<(), CpuError> {
    let out = cpu.i().unwrap().amode_output();

    let next_address = match out {
        Fetched { value: _, address } => address,
        AbsoluteAddress(next_address) => next_address,
        _ => return Err(CpuError::BadAddressing),
    };

    cpu.regset_mut().set_prog_counter(next_address);

    Ok(())
}

///
/// Jump to subroutine
///
/// Same as `jmp`, but the saves the return address first.
pub fn jsr(cpu: &mut Cpu) -> Result<(), CpuError> {
    let return_address = cpu.pc() - 1;
    cpu.stk_doublepush(return_address);

    jmp(cpu);

    Ok(())
}

///
/// Load accumulator with a memory value
///
/// **Zero:** If the value is 0\
/// **Negative:** If the value is negative
pub fn lda(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;

    let regs = cpu.regset_mut();
    regs.set_accumulator(fetched);

    regs.set_zero(fetched == 0);
    regs.set_negative(fetched & 0x80 > 0);

    Ok(())
}

///
/// Load X index with a memory value
///
/// **Zero:** If the value is 0\
/// **Negative:** If the value is negative
pub fn ldx(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;

    let regs = cpu.regset_mut();
    regs.set_x_index(fetched);

    regs.set_zero(fetched == 0);
    regs.set_negative(fetched & 0x80 > 0);

    Ok(())
}

///
/// Load X index with a memory value
///
/// **Zero:** If the value is 0\
/// **Negative:** If the value is negative
pub fn ldy(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;

    let regs = cpu.regset_mut();
    regs.set_y_index(fetched);

    regs.set_zero(fetched == 0);
    regs.set_negative(fetched & 0x80 > 0);

    Ok(())
}

///
/// Perform a bit shift right to a memory value or to the accumulator
///
/// **Zero:** If the resuling value is 0\
/// **Negative:** Always gets set to 0 \
/// **Carry:** Gets set to value of the lowest bit of the value before shifting
pub fn lsr(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched_u16 = verify_and_fetch(cpu)?;
    let fetched_shifted_u16 = fetched_u16 >> 1;
    let val = fetched_u16 as u8;
    let val_shifted = (fetched_shifted_u16 & 0x00FF) as u8;

    if let Some(i) = cpu.i() {
        if i.amode() == Imp {
            let _ = cpu.regset_mut().set_accumulator(val_shifted);
        } else if let Fetched { value: _, address } = i.amode_output() {
            cpu.writ_byte(address, val_shifted);
        } else {
            return Err(CpuError::BadAddressing);
        }
    }

    let regs = cpu.regset_mut();
    regs.set_carry((fetched_shifted_u16 & 0xFF00) > 0);
    regs.set_zero(val_shifted == 0);
    regs.set_negative(false);

    Ok(())
}

///
/// No operation - do nothing
pub fn nop(cpu: &mut Cpu) -> Result<(), CpuError> {
    Ok(())
}

///
/// Perform a bitwise "OR" between the accumulator and a memory value
pub fn ora(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;

    let a = cpu.regset().accumulator() | fetched;
    let regs = cpu.regset_mut();

    regs.set_accumulator(a);

    regs.set_zero(a == 0x00);
    regs.set_negative(a & 0x80 > 0);

    Ok(())
}

///
/// Push the accumulator on the stack
pub fn pha(cpu: &mut Cpu) -> Result<(), CpuError> {
    cpu.stk_push(cpu.regset().accumulator());

    Ok(())
}

///
/// Push the status register on the stack
///
/// Set the **Break** flag to true before that
pub fn php(cpu: &mut Cpu) -> Result<(), CpuError> {
    // Set Break flag and Unused flag to true before pushing
    let status = cpu.regset().status() | (1 << 3) | (1 << 4);
    cpu.stk_push(status);

    Ok(())
}

///
/// Pull the accumulator from the stack
///
/// **Zero:** If the pulled value is 0
/// **Negative:** If the pulled value is negative
pub fn pla(cpu: &mut Cpu) -> Result<(), CpuError> {
    let a = cpu.stk_pop();
    let regs = cpu.regset_mut();

    regs.set_accumulator(a);
    regs.set_zero(a == 0);
    regs.set_negative((a & 0x80) > 0);
    Ok(())
}

///
/// Pull the status register from the stack
///
/// **Note:** When looking through different sources of information there were some contrasting
/// on whether the **Break** flag should be cleared or not before the pulled value is assigned
/// to the status. In the current implementation, no changes are made between acquiring the value
/// from the stack and assigning it to the actual status register.
pub fn plp(cpu: &mut Cpu) -> Result<(), CpuError> {
    let p = cpu.stk_pop();
    let regs = cpu.regset_mut();
    regs.set_status(p);
    Ok(())
}

///
/// Rotate bit left either the accumulator or a memory value
///
/// **Zero:** If the value is equal to 0 \
/// **Negative:** If the value is negative \
/// **Carry:** 7th bit of the value
pub fn rol(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;

    let val = rol_inner(cpu, fetched);
    let amode_output = cpu.i().unwrap().amode_output();
    match amode_output {
        ValueOnly(_) => {
            let _ = cpu.regset_mut().set_accumulator(val);
        }
        Fetched { value: _, address } => {
            cpu.writ_byte(address, val);
        }
        _ => return Err(CpuError::BadAddressing),
    };

    fn rol_inner(cpu: &mut Cpu, fetched: u8) -> u8 {
        let regs = cpu.regset_mut();
        let carry = regs.carry() as u8;

        let val = fetched << 1 + carry;

        regs.set_zero(val == 0);
        regs.set_negative(val & 0x80 > 0);
        regs.set_carry(fetched & (1 << 7) > 0);
        val
    }

    Ok(())
}

///
/// Rotate bit right either the accumulator or a memory value
///
/// **Zero:** If the value is equal to 0 \
/// **Negative:** If the value is negative \
/// **Carry:** If the value is bigger than 255
pub fn ror(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)? as u8;

    let val = ror_inner(cpu, fetched);
    let amode_output = cpu.i().unwrap().amode_output();
    match amode_output {
        ValueOnly(_) => {
            let _ = cpu.regset_mut().set_accumulator(val);
        }
        Fetched { value: _, address } => {
            cpu.writ_byte(address, val);
        }
        _ => return Err(CpuError::BadAddressing),
    };

    fn ror_inner(cpu: &mut Cpu, fetched: u8) -> u8 {
        let regs = cpu.regset_mut();
        let carry = regs.carry() as u8;

        let val = fetched >> 1 + carry << 7;

        regs.set_zero(val == 0);
        regs.set_negative(val & 0x80 > 0);
        regs.set_carry(fetched & 1 > 0);
        val
    }

    Ok(())
}

///
/// Return from interrupt
///
/// All values of the status registers **may be** altered since the status register is pulled
/// from the stack.
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

///
/// Return from subroutine
///
pub fn rts(cpu: &mut Cpu) -> Result<(), CpuError> {
    let lo = cpu.stk_pop();
    let hi = cpu.stk_pop();
    let loaded_pc = Address::from_le_bytes([lo, hi]);

    cpu.regset_mut().set_prog_counter(loaded_pc.wrapping_add(1));

    Ok(())
}

///
/// Subtract from accumulator with borrow in
///
/// `A = A - M - (1 - C)`
///
/// **Zero:** If the result is 0 \
/// **Negative:** If the result is negative \
/// **Carry:** If the result is bigger than 255 \
/// **Overflowed:** If the result has overflowed by _sign_
pub fn sbc(cpu: &mut Cpu) -> Result<(), CpuError> {
    let fetched = verify_and_fetch(cpu)?;
    let fetched_inverted = fetched ^ 0x00FF;

    let regs = cpu.regset_mut();
    let accumulator_u16 = u16::from(regs.accumulator());
    let tmp: u16 = accumulator_u16 + fetched_inverted + u16::from(regs.carry());
    let accumulator = (tmp & 0x00FF) as u8;

    regs.set_carry(tmp > 0xFF);
    regs.set_zero(accumulator == 0);
    regs.set_negative((tmp & 0x80) > 0);
    regs.set_overflowed(
        (!(accumulator_u16 ^ fetched_inverted) & (accumulator_u16 ^ tmp) & 0x80) > 0,
    );

    regs.set_accumulator(accumulator);

    Ok(())
}

///
/// Set carry flag to true
///
pub fn sec(cpu: &mut Cpu) -> Result<(), CpuError> {
    cpu.regset_mut().set_carry(true);
    Ok(())
}

///
/// Set decimal mode flag to true
///
pub fn sed(cpu: &mut Cpu) -> Result<(), CpuError> {
    cpu.regset_mut().set_decimal_mode(true);
    Ok(())
}

///
/// Set interrupt disabled flag to true
///
pub fn sei(cpu: &mut Cpu) -> Result<(), CpuError> {
    cpu.regset_mut().set_irq_disabled(true);
    Ok(())
}

///
/// Store accumulator in memory
///
pub fn sta(cpu: &mut Cpu) -> Result<(), CpuError> {
    let _ = verify_and_fetch(cpu)?;

    let a = cpu.regset().accumulator();

    if let Some(i) = cpu.i() {
        if let Fetched { value: _, address } = i.amode_output() {
            cpu.writ_byte(address, a);
            return Ok(());
        } else {
            return Err(CpuError::BadAddressing);
        }
    }

    Err(CpuError::CurrentInstructionMissing)
}

///
/// Store X index in memory
///
pub fn stx(cpu: &mut Cpu) -> Result<(), CpuError> {
    let _ = verify_and_fetch(cpu)?;

    let x = cpu.regset().x_index();

    if let Some(i) = cpu.i() {
        if let Fetched { value: _, address } = i.amode_output() {
            cpu.writ_byte(address, x);
            return Ok(());
        } else {
            return Err(CpuError::BadAddressing);
        }
    }

    Err(CpuError::CurrentInstructionMissing)
}

///
/// Store Y index in memory
///
pub fn sty(cpu: &mut Cpu) -> Result<(), CpuError> {
    let _ = verify_and_fetch(cpu)?;

    let y = cpu.regset().y_index();

    if let Some(i) = cpu.i() {
        if let Fetched { value: _, address } = i.amode_output() {
            cpu.writ_byte(address, y);
            return Ok(());
        } else {
            return Err(CpuError::BadAddressing);
        }
    }

    Err(CpuError::CurrentInstructionMissing)
}

///
/// Transfer accumulator to Index X
///
/// **Zero:** If the accumulator value is 0\
/// **Negative:** If the accumulator value is negative
pub fn tax(cpu: &mut Cpu) -> Result<(), CpuError> {
    let regs = cpu.regset_mut();
    let a = regs.accumulator();
    regs.set_x_index(a);
    regs.set_negative(a & 0x80 > 0);
    regs.set_zero(a == 0);

    Ok(())
}

///
/// Transfer accumulator to Index Y
///
/// **Zero:** If the accumulator value is 0\
/// **Negative:** If the accumulator value is negative
pub fn tay(cpu: &mut Cpu) -> Result<(), CpuError> {
    let regs = cpu.regset_mut();
    let a = regs.accumulator();
    regs.set_y_index(a);
    regs.set_negative(a & 0x80 > 0);
    regs.set_zero(a == 0);

    Ok(())
}

///
/// Transfer stack pointer to Index X
///
/// **Zero:** If the stack pointer is 0
/// **Negative:** If the stack pointer is negative
pub fn tsx(cpu: &mut Cpu) -> Result<(), CpuError> {
    let regs = cpu.regset_mut();
    let stk_ptr = regs.stk_ptr();
    regs.set_x_index(stk_ptr);
    regs.set_negative(stk_ptr & 0x80 > 0);
    regs.set_zero(stk_ptr == 0);

    Ok(())
}

///
/// Transfer X index to the accumulator
///
/// **Zero:** If the X index value is 0
/// **Negative:** If the X index value is negative
pub fn txa(cpu: &mut Cpu) -> Result<(), CpuError> {
    let regs = cpu.regset_mut();
    let x = regs.x_index();
    regs.set_accumulator(x);
    regs.set_negative(x & 0x80 > 0);
    regs.set_zero(x == 0);

    Ok(())
}

///
/// Transfer X index to the stack pointer
///
/// **Zero:** If the X index value is 0
/// **Negative:** If the X index value is negative
pub fn txs(cpu: &mut Cpu) -> Result<(), CpuError> {
    let regs = cpu.regset_mut();
    let x = regs.x_index();
    regs.set_stk_ptr(x);

    Ok(())
}

///
/// Transfer Y index to the accumulator
///
/// **Zero:** If the Y index value is 0
/// **Negative:** If the Y index value is negative
pub fn tya(cpu: &mut Cpu) -> Result<(), CpuError> {
    let regs = cpu.regset_mut();
    let y = regs.y_index();
    regs.set_accumulator(y);
    regs.set_negative(y & 0x80 > 0);
    regs.set_zero(y == 0);

    Ok(())
}
