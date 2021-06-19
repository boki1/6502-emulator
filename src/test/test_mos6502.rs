#[cfg(test)]
mod test {
    use crate::mos6502::AddressingOutput::*;
    use crate::mos6502::InterruptKind::*;
    use crate::mos6502::*;
    use std::cell::RefCell;
    use std::rc::Rc;

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
        use crate::mos6502_addressing_modes::relative_am;
        use crate::mos6502_instruction_set::bpl;

        let i = Instruction::decode_by(0x10);

        assert_eq!(i.mnemonic(), "bpl".to_string());
        assert_eq!(i.time(), 2);
        assert_eq!(i.size(), 2);
        assert_eq!(i.fun() as usize, bpl as usize);
        assert_eq!(i.amode_fun() as usize, relative_am as usize);
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

        let old_pc = cpu.load_program(&prog, 0x8000, prog.len(), true);
        assert_eq!(old_pc.ok(), Some(0x1000));

        let read = cpu.read_some(0x8000, prog.len() as u16);
        assert_eq!(read, prog);
    }

    #[test]
    fn test_load_program_and_disassemble() {
        let mut cpu = Cpu::new_custompc(0x1000);
        cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
        let prog: Vec<Byte> = vec![162, 10, 142, 0, 0, 162, 3];
        let old_pc = cpu.load_program(&prog, 0x8000, 7, true);
        let expected_asm = Asm::new(vec![
            Instruction::decode_by(162),
            Instruction::decode_by(142),
            Instruction::decode_by(162),
        ]);

        let asm = cpu.disassemble(0x8000, 7);

        assert_eq!(old_pc, Ok(0x1000));
        assert_eq!(asm, Some(expected_asm));
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
        assert_eq!(cpu.time().residual(), 0);
        assert_eq!(cpu.pc(), 0x2002);
        assert_eq!(cpu.regset().irq_disabled(), true);
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
        assert_eq!(cpu.time().residual(), 0);
        assert_eq!(cpu.pc(), 0x0000);
        assert_eq!(cpu.regset().irq_disabled(), true);
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
        assert_eq!(cpu.time().residual(), 0);
        assert_eq!(cpu.pc(), 0x2002);
        assert_eq!(cpu.regset().irq_disabled(), true);
    }

    fn setup(custom_pc: Word, connect: bool, opcode: Opcode, operand: Option<Word>) -> Cpu {
        let mut cpu = Cpu::new_custompc(custom_pc);

        cpu.i_decode_and_set(opcode, operand);

        if connect {
            cpu.connect_to(Rc::new(RefCell::new(MainBus::new())));
        }

        cpu
    }

    #[test]
    fn test_prepare_operands_zero() {
        let mut cpu = setup(0x0001, true, 0x00, None);
        cpu.i_decode_and_set(0x8, None);

        load_operand_curr_i(&mut cpu);

        let i = cpu.i().unwrap();
        assert_eq!(i.operand(), None);
        assert_eq!(i.loaded_from(), 0x0);
    }

    #[test]
    fn test_prepare_operands_one() {
        let mut cpu = setup(0x001, true, 0x00, None);
        cpu.i_decode_and_set(0xa9, None);

        load_operand_curr_i(&mut cpu);

        assert_eq!(cpu.pc(), 0x02);
        let i = cpu.i().unwrap();
        assert_eq!(i.operand().is_some(), true);
        assert_eq!(i.operand().unwrap(), 0x0);
        assert_eq!(i.loaded_from(), 0x0);
    }

    #[test]
    fn test_prepare_operands_two() {
        let mut cpu = setup(0x001, true, 0x00, None);
        cpu.i_decode_and_set(0xad, None);
        cpu.writ_byte(0x1, 0x10);
        cpu.writ_byte(0x2, 0x11);

        load_operand_curr_i(&mut cpu);

        assert_eq!(cpu.pc(), 0x03);
        let i = cpu.i().unwrap();
        assert_eq!(i.operand().is_some(), true);
        assert_eq!(i.operand().unwrap(), 0x1110);
        assert_eq!(i.loaded_from(), 0x0);
    }

    #[test]
    fn test_prepare_operands_zero_custom_i() {
        let mut cpu = setup(0xFEBE, true, 0xA9, Some(0x10));
        cpu.writ_byte(0x1002, 0xEE);
        cpu.writ_byte(0x1003, 0xFF);
        let mut i = Instruction::decode_by(0x08);
        load_operand(&mut cpu, &mut i, 0x1001);

        assert_eq!(cpu.pc(), 0xFEBE);
        assert_eq!(*cpu.i().unwrap(), Instruction::decode_by(0xA9));
        assert_eq!(i.operand(), None);
        assert_eq!(i.amode_output(), NotExecuted);
        assert_eq!(i.loaded_from(), 0x1001);
    }

    #[test]
    fn test_prepare_operands_one_custom_i_1() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1002, 0x10);
        let mut i = Instruction::decode_by(0xA9);

        load_operand(&mut cpu, &mut i, 0x1001);

        assert_eq!(cpu.pc(), 0xFEBE);
        assert_eq!(*cpu.i().unwrap(), Instruction::decode_by(0x08));
        assert_eq!(i.operand(), Some(0x10));
        assert_eq!(i.amode_output(), NotExecuted);
        assert_eq!(i.loaded_from(), 0x1001);
    }

    #[test]
    fn test_prepare_operands_one_custom_i_2() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1002, 0x20);
        let mut i = Instruction::decode_by(0x19);

        load_operand(&mut cpu, &mut i, 0x1001);

        assert_eq!(cpu.pc(), 0xFEBE);
        assert_eq!(*cpu.i().unwrap(), Instruction::decode_by(0x08));
        assert_eq!(i.operand(), Some(0x20));
        assert_eq!(i.amode_output(), NotExecuted);
        assert_eq!(i.loaded_from(), 0x1001);
    }

    #[test]
    fn test_prepare_operands_two_custom_i() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1002, 0x20);
        cpu.writ_byte(0x1003, 0x40);
        let mut i = Instruction::decode_by(0xCC);

        load_operand(&mut cpu, &mut i, 0x1001);

        assert_eq!(cpu.pc(), 0xFEBE);
        assert_eq!(*cpu.i().unwrap(), Instruction::decode_by(0x08));
        assert_eq!(i.operand(), Some(0x4020));
        assert_eq!(i.amode_output(), NotExecuted);
        assert_eq!(i.loaded_from(), 0x1001);
    }

    #[test]
    fn test_stringify_disassembly() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x00);
        cpu.writ_byte(0x1001, 0xA9);
        cpu.writ_byte(0x1002, 0x10);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from("0x1000\tbrk\t\t; Imp\n");
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
                0x1000\tbrk\t\t; Imp\n\
                0x1001\tlda\t#0x10\t; Imm\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringift_disassembly_zero_page_and_absolute() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x6D);
        cpu.writ_byte(0x1001, 0xAA);
        cpu.writ_byte(0x1002, 0xBB);
        cpu.writ_byte(0x1003, 0x65);
        cpu.writ_byte(0x1004, 0x30);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 5);

        let expected_str = String::from(
            "\
                0x1000\tadc\t0xbbaa\t; Abs\n\
                0x1003\tadc\t0x30\t; Zp0\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringift_disassembly_zero_page_and_absolute_offset_x() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x7D);
        cpu.writ_byte(0x1001, 0xAA);
        cpu.writ_byte(0x1002, 0xBB);
        cpu.writ_byte(0x1003, 0x75);
        cpu.writ_byte(0x1004, 0x30);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 5);

        let expected_str = String::from(
            "\
                0x1000\tadc\t0xbbaa, X\t; Abx\n\
                0x1003\tadc\t0x30, X\t; Zpx\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringift_disassembly_zero_page_and_absolute_offset_y() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0xB9);
        cpu.writ_byte(0x1001, 0xAA);
        cpu.writ_byte(0x1002, 0xBB);
        cpu.writ_byte(0x1003, 0xB6);
        cpu.writ_byte(0x1004, 0x30);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 5);

        let expected_str = String::from(
            "\
                0x1000\tlda\t0xbbaa, Y\t; Aby\n\
                0x1003\tldx\t0x30, Y\t; Zpy\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringift_disassembly_indirect() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x6C);
        cpu.writ_byte(0x1001, 0xAA);
        cpu.writ_byte(0x1002, 0xBB);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from(
            "\
                0x1000\tjmp\t(0xbbaa)\t; Ind\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringift_disassembly_relative() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x90);
        cpu.writ_byte(0x1001, 0x80);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from(
            "\
                0x1000\tbcc\t0x80\t; Rel\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringift_disassembly_indirect_offset_y() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x91);
        cpu.writ_byte(0x1001, 0x10);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from(
            "\
                0x1000\tsta\t(0x10), Y\t; Iny\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }

    #[test]
    fn test_stringift_disassembly_indirect_offset_x() {
        let mut cpu = setup(0xFEBE, true, 0x08, None);
        cpu.writ_byte(0x1000, 0x81);
        cpu.writ_byte(0x1001, 0x10);

        let str_res = Asm::stringify_range(&mut cpu, 0x1000, 1);

        let expected_str = String::from(
            "\
                0x1000\tsta\t(0x10, X)\t; Inx\n\
        ",
        );
        assert_eq!(str_res.ok(), Some(expected_str));
    }
}
