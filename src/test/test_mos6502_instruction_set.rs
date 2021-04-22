use crate::mos6502::{
    Address, AddressingOutput, AddressingOutput::*, Byte, Cpu, Instruction, MainBus, Opcode,
    RegisterSet, Word, BRK_VECTOR,
};
use crate::mos6502_instruction_set::*;

#[cfg(test)]
mod test {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    fn setup(custom_pc: Word, connect: bool, opcode: Option<Opcode>, operand: Option<Word>) -> Cpu {
        let mut cpu = Cpu::new_custompc(custom_pc);

        if let Some(opc) = opcode {
            cpu.i_decode_and_set(opc, operand);
        }

        if connect {
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
        }

        cpu
    }

    #[test]
    fn test_adc_regular() {
        let mut cpu = setup(0x0000, true, Some(0x69), Some(0x10));
        cpu.i_mut().unwrap().set_amode_output(ValueOnly(0x10));

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
        cpu.i_mut().unwrap().set_amode_output(ValueOnly(0x80));

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
        cpu.i_mut().unwrap().set_amode_output(ValueOnly(0x00));

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
        cpu.i_mut().unwrap().set_amode_output(ValueOnly(0x01));
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
        cpu.i_mut().unwrap().set_amode_output(ValueOnly(0x01));
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
        cpu.i_mut().unwrap().set_amode_output(ValueOnly(0x00));
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
        cpu.i_mut().unwrap().set_amode_output(ValueOnly(0x80));
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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x4F,
            address: 0xFF,
        });
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
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x4));

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

        assert_eq!(cpu.pc(), 0xFEBE + 10);
        assert_eq!(cpu.time().residual(), 1);
    }

    #[test]
    fn test_do_branch_negative() {
        let mut cpu = setup(0xFEBE, true, None, None);
        *cpu.time_mut().residual_mut() = 0;

        do_branch(&mut cpu, 0xFEBE - 120);

        assert_eq!(cpu.pc(), 0xFEBE - 120);
        assert_eq!(cpu.time().residual(), 1);
    }

    #[test]
    fn test_do_branch_page_cross() {
        let mut cpu = setup(0xFEFE, true, None, None);
        *cpu.time_mut().residual_mut() = 0;

        do_branch(&mut cpu, 0xFEFE + 10);

        assert_eq!(cpu.pc(), 0xFEFE + 10);
        assert_eq!(cpu.time().residual(), 2);
    }

    #[test]
    fn test_do_branch_negative_page_cross() {
        let mut cpu = setup(0xFEFE + 10, true, None, None);
        *cpu.time_mut().residual_mut() = 0;

        do_branch(&mut cpu, 0xFEFE);

        assert_eq!(cpu.pc(), 0xFEFE);
        assert_eq!(cpu.time().residual(), 2);
    }

    #[test]
    fn test_bit() {
        let mut cpu = setup(0xFEBE, true, Some(0x24), Some(0x10));
        cpu.regset_mut().set_accumulator(0x07);
        cpu.i_mut().unwrap().set_amode_output(Fetched {
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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x12,
            address: 0x10 + 0x13,
        });

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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x11,
            address: 0xEE,
        });

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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x11,
            address: 0xCD,
        });

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

        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x10));
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

        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x80,
            address: 0xFE,
        });
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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x00,
            address: 0xFE,
        });

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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x10,
            address: 0xFE,
        });

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
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x09));

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
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0xFF));

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
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x7F));

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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x7E,
            address: 0xFE,
        });

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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x7F,
            address: 0xFE,
        });

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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0xFF,
            address: 0xFE,
        });

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
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x14,
            address: 0xAE,
        });

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
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x00));
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
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x00));
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

    #[test]
    fn test_tax() {
        let mut cpu = setup(0xFEBE, true, None, None);
        let regs = cpu.regset_mut();
        regs.set_x_index(0xAA);
        regs.set_accumulator(0x11);

        let res_tax = tax(&mut cpu);

        let regs = cpu.regset();
        assert_eq!(res_tax.ok(), Some(()));
        assert_eq!(regs.x_index(), 0x11);
        assert_eq!(regs.accumulator(), 0x11);
    }

    #[test]
    fn test_txa() {
        let mut cpu = setup(0xFEBE, true, None, None);
        let regs = cpu.regset_mut();
        regs.set_x_index(0xAA);
        regs.set_accumulator(0x11);

        let res_txa = txa(&mut cpu);

        let regs = cpu.regset();
        assert_eq!(res_txa.ok(), Some(()));
        assert_eq!(regs.x_index(), 0xaa);
        assert_eq!(regs.accumulator(), 0xAA);
    }

    #[test]
    fn test_tay() {
        let mut cpu = setup(0xFEBE, true, None, None);
        let regs = cpu.regset_mut();
        regs.set_y_index(0xAA);
        regs.set_accumulator(0x11);

        let res_tay = tay(&mut cpu);

        let regs = cpu.regset();
        assert_eq!(res_tay.ok(), Some(()));
        assert_eq!(regs.y_index(), 0x11);
        assert_eq!(regs.accumulator(), 0x11);
    }

    #[test]
    fn test_tya() {
        let mut cpu = setup(0xFEBE, true, None, None);
        let regs = cpu.regset_mut();
        regs.set_y_index(0xAA);
        regs.set_accumulator(0x11);

        let res_tya = tya(&mut cpu);

        let regs = cpu.regset();
        assert_eq!(res_tya.ok(), Some(()));
        assert_eq!(regs.y_index(), 0xAA);
        assert_eq!(regs.accumulator(), 0xAA);
    }

    #[test]
    fn test_txs() {
        let mut cpu = setup(0xFEBE, true, None, None);
        let regs = cpu.regset_mut();
        regs.set_x_index(0xAA);
        regs.set_stk_ptr(0x11);

        let res_txs = txs(&mut cpu);

        let regs = cpu.regset();
        assert_eq!(res_txs.ok(), Some(()));
        assert_eq!(regs.x_index(), 0xAA);
        assert_eq!(regs.stk_ptr(), 0xAA);
    }

    #[test]
    fn test_tsx() {
        let mut cpu = setup(0xFEBE, true, None, None);
        let regs = cpu.regset_mut();
        regs.set_x_index(0xAA);
        regs.set_stk_ptr(0x11);

        let res_tsx = tsx(&mut cpu);

        let regs = cpu.regset();
        assert_eq!(res_tsx.ok(), Some(()));
        assert_eq!(regs.x_index(), 0x11);
        assert_eq!(regs.stk_ptr(), 0x11);
    }

    #[test]
    fn test_jmp() {
        let mut cpu = setup(0xFEBE, true, Some(0x6C), Some(0x1010));
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(AbsoluteAddress(0x1010));

        let res = jmp(&mut cpu);

        assert_eq!(res.ok(), Some(()));
        assert_eq!(cpu.pc(), 0x1010);
    }

    #[test]
    fn test_sbc_regular() {
        let mut cpu = setup(0x0000, true, Some(0x69), Some(0x10));
        cpu.regset_mut().set_accumulator(0x20);
        cpu.regset_mut().set_carry(true);
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x10));

        let res = sbc(&mut cpu);

        assert_eq!(res.ok(), Some(()));
        let regs = cpu.regset();
        assert_eq!(regs.accumulator(), 0x10);
        assert_eq!(regs.carry(), true);
        assert_eq!(regs.zero(), false);
        assert_eq!(regs.negative(), false);
        assert_eq!(regs.overflowed(), false);
    }

    #[test]
    fn test_sbc_negative() {
        let mut cpu = setup(0x0000, true, Some(0x69), Some(0x80));
        cpu.regset_mut().set_accumulator(0x20);
        cpu.regset_mut().set_carry(true);
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x80));

        let res = sbc(&mut cpu);

        assert_eq!(res.ok(), Some(()));
        let regs = cpu.regset();
        assert_eq!(regs.accumulator(), 0xFF - 0x60 + 1);
        assert_eq!(regs.carry(), false);
        assert_eq!(regs.zero(), false);
        assert_eq!(regs.negative(), true);
        assert_eq!(regs.overflowed(), true);
    }

    #[test]
    fn test_sbc_zero() {
        let mut cpu = setup(0x0000, true, Some(0x69), Some(0x00));
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x00));
        cpu.regset_mut().set_carry(true);

        let res = sbc(&mut cpu);

        assert_eq!(res.ok(), Some(()));
        let regs = cpu.regset();
        assert_eq!(regs.accumulator(), 0x0);
        assert_eq!(regs.carry(), true);
        assert_eq!(regs.zero(), true);
        assert_eq!(regs.negative(), false);
        assert_eq!(regs.overflowed(), false);
    }

    #[test]
    fn test_sbc_overflowed() {
        let mut cpu = setup(0x0000, true, Some(0x69), Some(0x01));
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x01));
        cpu.regset_mut().set_accumulator(0x00);

        let res = sbc(&mut cpu);

        assert_eq!(res.ok(), Some(()));
        let regs = cpu.regset();
        assert_eq!(regs.accumulator(), 0xFF - 1);
        assert_eq!(regs.carry(), false);
        assert_eq!(regs.zero(), false);
        assert_eq!(regs.negative(), true);
        assert_eq!(regs.overflowed(), false);
    }

    #[test]
    fn test_rol() {
        let mut cpu = setup(0x0000, true, Some(0x69), Some(0x01));
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x80,
            address: 0x00,
        });
        let regs = cpu.regset_mut();
        regs.set_zero(false);
        regs.set_carry(false);
        regs.set_negative(true);

        let res = rol(&mut cpu);

        assert_eq!(res.ok(), Some(()));
        let regs = cpu.regset();
        assert_eq!(regs.accumulator(), 0x00);
        assert_eq!(regs.zero(), true);
        assert_eq!(regs.carry(), true);
        assert_eq!(regs.negative(), false);
    }

    #[test]
    fn test_rol_acc() {
        let mut cpu = setup(0x0000, true, Some(0x69), Some(0x01));
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x80));
        let regs = cpu.regset_mut();
        regs.set_zero(false);
        regs.set_carry(false);
        regs.set_negative(true);
        regs.set_accumulator(0x80);

        let res = rol(&mut cpu);

        assert_eq!(res.ok(), Some(()));
        let regs = cpu.regset();
        assert_eq!(regs.accumulator(), 0x00);
        assert_eq!(regs.zero(), true);
        assert_eq!(regs.carry(), true);
        assert_eq!(regs.negative(), false);
    }

    #[test]
    fn test_ror() {
        let mut cpu = setup(0x0000, true, Some(0x69), Some(0x01));
        cpu.i_mut().unwrap().set_amode_output(Fetched {
            value: 0x1,
            address: 0x0000,
        });
        let regs = cpu.regset_mut();
        regs.set_zero(false);
        regs.set_carry(false);
        regs.set_negative(true);
        regs.set_accumulator(0x00);

        let res = ror(&mut cpu);

        assert_eq!(res.ok(), Some(()));
        let regs = cpu.regset();
        assert_eq!(regs.accumulator(), 0x00);
        assert_eq!(regs.zero(), true);
        assert_eq!(regs.carry(), true);
        assert_eq!(regs.negative(), false);
    }

    #[test]
    fn test_ror_acc() {
        let mut cpu = setup(0x0000, true, Some(0x69), Some(0x01));
        cpu.i_mut()
            .as_mut()
            .unwrap()
            .set_amode_output(ValueOnly(0x1));
        let regs = cpu.regset_mut();
        regs.set_zero(false);
        regs.set_carry(false);
        regs.set_negative(true);
        regs.set_accumulator(0x01);

        let res = ror(&mut cpu);

        assert_eq!(res.ok(), Some(()));
        let regs = cpu.regset();
        assert_eq!(regs.accumulator(), 0x00);
        assert_eq!(regs.zero(), true);
        assert_eq!(regs.carry(), true);
        assert_eq!(regs.negative(), false);
    }
}
