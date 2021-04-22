use crate::mos6502::{
    Address, AddressingMode, AddressingModeFn, AddressingOutput, AddressingOutput::*, Byte, Cpu,
    CpuError, CpuError::*, Instruction, MainBus, Opcode, Word,
};

use std::cell::RefCell;
use std::rc::Rc;

//
// Addressing modes
// The 6502 cpu support different kinds of _addressing mode_/
//
//
//
// All addressing mode functions **require** that
// the cpu has loaded a value for `current_instruction`
// and its concrete operands. Each of them will check
// for the operands they expect, but that there is
// a `current_instruction`. A `panic!()` (or appropriate
// error return) will occur if **some** of these
// requirements are not met.
//

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
/// **Implied**
/// This addressing mode is the simplest one, because it is
/// use only with instructions which do not need any additional
/// context -- it is _implied_.
///
///
///  No operands here. This addressing mode does not use any
///  additional values. In this current implementation
///  the accumulator addressing mode is "simulated" by
///  calling `implied_am`. The only difference between the
///  two is that the accumulator AM is using the
///  value of the accumulator as its operand, hence the name --
///  accumulator. That is why the contents of the
///  accumulator are set as this addressing mode's output.
///
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
    let i = cpu.i().unwrap();
    if let Some(operand) = i.operand() {
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
    let i = cpu.i().unwrap();
    if let Some(mut operand) = i.operand() {
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
    let i = cpu.i().unwrap();
    if let Some(operand) = i.operand() {
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
    let i = cpu.i().unwrap();
    if let Some(ptr) = i.operand() {
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
    let i = cpu.i().unwrap();

    if let Some(base_u16) = i.operand() {
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
    let i = cpu.i().unwrap();

    if let Some(base_u16) = i.operand() {
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
