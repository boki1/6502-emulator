#[cfg(test)]
mod test {
    use crate::mos6502::{
        Address, AddressingOutput, AddressingOutput::*, Byte, Cpu, Instruction, MainBus, Opcode,
        Operand, RegisterSet, Word, BRK_VECTOR,
    };
    use crate::mos6502_addressing_modes::*;
    use crate::mos6502_instruction_set::*;

    use std::cell::RefCell;
    use std::rc::Rc;

    // use super::*;

    /// A setup() routine for all tests.
    fn setup(custom_pc: Word, connect: bool, opcode: Opcode, operand: Operand) -> Cpu {
        let mut cpu = Cpu::new_custompc(custom_pc);

        cpu.i_decode_and_set(opcode, operand);

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
                address: 0x35,
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
                address: 0x35 + 3,
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
                address: 0x35 - 1,
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
                address: 0x210,
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
                address: 0x210 + 0xA,
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
                address: 0x2074,
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
                address: 0xA701,
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
                address: 0xFE01,
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
        let marked_extra_cycle = cpu.time().residual() == 1;

        assert_eq!(
            result.ok(),
            Some(Fetched {
                value: 0x34,
                address: 0xA80F,
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
        let marked_extra_cycle = cpu.time().residual() == 1;

        assert_eq!(
            result.ok(),
            Some(Fetched {
                value: 0x34,
                address: 0xA711,
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
        let marked_extra_cycle = cpu.time().residual() == 1;

        assert_eq!(
            result.ok(),
            Some(Fetched {
                value: 0x37,
                address: 0x4038,
            })
        );
        assert_eq!(marked_extra_cycle, false);
    }
}
