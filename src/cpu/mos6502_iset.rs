use log::*;

use super::mos6502::{mos6502_addressing_modes::AddrMode, Cpu, Flag, Vectors, STACK_BASE};

impl Cpu {
    /// add memory to accumulator with carry
    /// a + m + c -> a, c
    /// modifies N, Z, C and V
    pub fn adc(&mut self) {
        let result: u16 = self.fetched as u16 + self.state.a as u16 + self.flagv(Flag::C) as u16;

        self.flag(Flag::C, result > 255);

        let is_empty = result & 0xff == 0;
        self.flag(Flag::Z, is_empty);

        let is_neg = result & 0x80 != 0;
        self.flag(Flag::N, is_neg);

        let acc_16: u16 = self.state.a as u16;
        let addend_16: u16 = self.fetched as u16;
        let overflowed = (!(acc_16 ^ addend_16) & ((acc_16 & result) & 0x80)) != 0;

        self.flag(Flag::V, overflowed);

        // Here whether additional_cycle may appear is computed in when addressing memory for the operand
        self.additional_cycle = true;
        self.state.a = (result & 0x00ff) as u8;
    }

    /// and memory with accumulator
    /// modifies N, Z
    pub fn and(&mut self) {
        self.state.a &= self.fetched;

        if self.state.a == 0 {
            self.flag_raise(Flag::Z);
        }

        if self.state.a & 0x80 != 0 {
            self.flag_raise(Flag::N);
        }

        self.additional_cycle = false;
    }

    /// shift left one byte
    ///     - can be used for memory and accumulator
    /// modifies N, Z, C
    pub fn asl(&mut self) {
        self.flag(Flag::C, self.fetched & 0x1 != 0);
        let res = self.state.a << 1;
        self.flag(Flag::Z, res == 0);
        self.flag(Flag::N, res & 0x80 != 0);

        if self.current.is_none() {
            error!("CURRENT IS EMPTY.");
        }

        let i = self.current.unwrap();

        if i.am == AddrMode::Implied || i.am == AddrMode::Accumulator {
            self.state.a = res;
        } else {
            let aa = self.addr_abs;
            self.writ_byte(aa, res);
        }
    }

    /// Performs the common behaviour of all branch instructions.
    /// Evaluate the final address of the operand, check for page crossing and update the PC.
    #[inline]
    fn branch_instr_common(&mut self) {
        //print!("{} rel to {} => ", self.addr_rel, self.addr_abs);
        self.addr_abs = self.addr_rel.wrapping_add(self.state.pc);
        //println!("{}", self.addr_abs);
        self.additional_cycle = Cpu::same_page(self.addr_abs, self.state.pc);
        self.state.pc = self.addr_abs;
    }

    /// branch on carry clear
    pub fn bcc(&mut self) {
        if self.flagv(Flag::C) == false {
            self.branch_instr_common();
        }
    }

    /// branch on carry set
    pub fn bcs(&mut self) {
        if self.flagv(Flag::C) == true {
            self.branch_instr_common();
        }
    }

    /// branch on zero set
    pub fn beq(&mut self) {
        if self.flagv(Flag::Z) {
            self.branch_instr_common();
        }
    }

    /// test bits in memory with accumulator
    /// modifies N, V, Z
    pub fn bit(&mut self) {
        let conjunction = self.state.a & self.fetched;
        if conjunction == 0 {
            self.flag_raise(Flag::Z);
        }

        let n = self.fetched & 0x80 != 0;
        self.flag(Flag::N, n);

        let v = self.fetched & 0x40 != 0;
        self.flag(Flag::V, v);
    }

    /// branch on result minus
    pub fn bmi(&mut self) {
        if self.flagv(Flag::N) {
            self.branch_instr_common();
        }
    }

    /// branch on result not zero
    pub fn bne(&mut self) {
        if !self.flagv(Flag::Z) {
            self.branch_instr_common();
        }
    }

    /// branch on result plus
    pub fn bpl(&mut self) {
        if self.flagv(Flag::N) {
            self.branch_instr_common();
        }
    }

    /// force break - software interrupt
    /// The difference with IRQ is that the B flag is set.
    /// Other then that the status registers is pushed onto the stack as well as the program
    /// counter.
    pub fn brk(&mut self) {
        let mut skptr_u16 = self.state.sp as u16;
        let mut pc_temp = self.state.pc;
        self.writ_word(&mut skptr_u16, pc_temp);
        let p = self.flag_raise(Flag::B);
        self.writ_byte(skptr_u16, p);

        self.state.sp = skptr_u16 as u8;

        self.flag_drop(Flag::I);
        let mut load_addr = Vectors::IRQ as u16;
        let lo = self.read_and_inc(&mut load_addr) as u16;
        let hi = self.read_bus(load_addr) as u16;
        pc_temp = hi << 8 | lo;

        self.state.pc = pc_temp;
    }

    /// branch on overflow clear
    pub fn bvc(&mut self) {
        if self.flagv(Flag::V) == false {
            self.branch_instr_common();
        }
    }

    /// branch on overflow set
    pub fn bvs(&mut self) {
        if self.flagv(Flag::V) == true {
            self.branch_instr_common();
        }
    }

    /// clear carry flag
    pub fn clc(&mut self) {
        self.flag_drop(Flag::C);
    }

    /// clear decimal mode
    pub fn cld(&mut self) {
        self.flag_drop(Flag::D);
    }

    /// clear interrupts disabled bit
    pub fn cli(&mut self) {
        self.flag_drop(Flag::I);
    }

    /// clear overflow bit
    pub fn clv(&mut self) {
        self.flag_drop(Flag::V);
    }

    /// common behaviour of compare instructions
    #[inline]
    fn compare_instr_common(&mut self, reg: u8) {
        if reg >= self.fetched {
            self.flag_raise(Flag::C);
        }

        if reg == self.fetched {
            self.flag_raise(Flag::Z);
        }

        if reg < self.fetched {
            self.flag_raise(Flag::N);
        }
    }

    /// compare memory with accumulator
    pub fn cmp(&mut self) {
        self.compare_instr_common(self.state.a);
    }

    /// compare memory with x index
    pub fn cpx(&mut self) {
        self.compare_instr_common(self.state.x);
    }

    /// compare memory with y index
    pub fn cpy(&mut self) {
        self.compare_instr_common(self.state.y);
    }

    /// decrement memory or accumulator
    pub fn dec(&mut self) {
        let new_val = self.fetched - 1;
        let aa = self.addr_abs;
        self.writ_byte(aa, new_val);

        if new_val & 0x80 != 0 {
            self.flag_raise(Flag::N);
        }

        if new_val == 0 {
            self.flag_raise(Flag::Z);
        }
    }

    /// decrement x index
    pub fn dex(&mut self) {
        self.state.x.wrapping_sub(1);

        let x = self.state.x;
        self.flag(Flag::N, x & 0x80 != 0);
        self.flag(Flag::Z, x == 0);
    }

    /// decrement y index
    pub fn dey(&mut self) {
        self.state.y.wrapping_sub(1);

        let y = self.state.y;
        self.flag(Flag::N, y & 0x80 != 0);
        self.flag(Flag::Z, y == 0);
    }

    /// exclusive-or on accumulator
    pub fn eor(&mut self) {
        self.state.a ^= self.fetched;
        self.flag(Flag::Z, self.state.a == 0);
        self.flag(Flag::N, self.state.a & 0x80 != 0);
    }

    /// increment memory
    pub fn inc(&mut self) {
        let new_val = self.fetched + 1;
        let aa = self.addr_abs;
        self.writ_byte(aa, new_val);

        self.flag(Flag::N, new_val & 0x80 != 0);
        self.flag(Flag::Z, new_val == 0);
    }

    /// increment x index
    pub fn inx(&mut self) {
        self.state.x.wrapping_add(1);

        let x = self.state.x;
        self.flag(Flag::N, x & 0x80 != 0);
        self.flag(Flag::Z, x == 0);
    }

    /// increment y index
    pub fn iny(&mut self) {
        self.state.y.wrapping_add(1);

        let y = self.state.y;
        self.flag(Flag::N, y & 0x80 != 0);
        self.flag(Flag::Z, y == 0);
    }

    /// jump to new location
    pub fn jmp(&mut self) {
        self.state.pc = self.addr_abs;
    }

    /// jump to subroutine
    pub fn jsr(&mut self) {
        self.state.pc -= 1;
        let mut sk_addr = STACK_BASE + self.state.sp as u16;
        let to_write = self.state.pc;
        self.writ_word(&mut sk_addr, to_write);
        self.state.sp -= 2;

        self.state.pc = self.addr_abs;
    }

    /// common behaviour of all load functions
    #[inline]
    fn ld_instr_common(&mut self, reg_ref: &mut u8) {
        *reg_ref = self.fetched;
        self.flag(Flag::N, *reg_ref & 0x80 != 0);
        self.flag(Flag::Z, *reg_ref == 0);
    }

    /// load accumulator with memory
    pub fn lda(&mut self) {
        let mut ac = self.state.a;
        self.ld_instr_common(&mut ac);
        self.state.a = ac;
    }

    /// load index x with memory
    pub fn ldx(&mut self) {
        let mut xx = self.state.x;
        self.ld_instr_common(&mut xx);
        self.state.x = xx;
    }

    /// load index y with memory
    pub fn ldy(&mut self) {
        let mut yy = self.state.y;
        self.ld_instr_common(&mut yy);
        self.state.y = yy;
    }

    /// shift right by one bit
    /// memory or accumulator
    pub fn lsr(&mut self) {
        self.flag(Flag::C, self.fetched & 0x1 != 0);
        let res = self.state.a >> 1;
        self.flag(Flag::Z, res == 0);
        self.flag(Flag::N, res & 0x80 != 0);

        if self.current.is_none() {
            error!("CURRENT IS EMPTY.");
        }

        let i = self.current.unwrap();

        if i.am == AddrMode::Implied || i.am == AddrMode::Accumulator {
            self.state.a = res;
        } else {
            let aa = self.addr_abs;
            self.writ_byte(aa, res);
        }
    }

    /// no operation
    pub fn nop(&mut self) {
        if let Some(i) = &self.current {
            // there is some difference in the `nop`'s (illegal opcodes stuff)
            self.additional_cycle = match i.opcode {
                0x1c => true,
                0x3c => true,
                0x5c => true,
                0x7c => true,
                0xdc => true,
                0xfc => true,
                _ => false,
            };
        }
    }

    /// or memory with accumulator
    pub fn ora(&mut self) {
        self.state.a |= self.fetched;

        self.flag(Flag::Z, self.state.a == 0);
        self.flag(Flag::N, self.state.a & 0x80 != 0x80);
    }

    /// push accumulator to the stack
    pub fn pha(&mut self) {
        let sk_addr = STACK_BASE + self.state.sp as u16;
        let ac = self.state.a;
        self.writ_byte(sk_addr, ac);

        self.state.sp -= 1;
    }

    /// push cpu status register to the stack
    pub fn php(&mut self) {
        let sk_addr = STACK_BASE + self.state.sp as u16;
        self.flag_raise(Flag::U);
        self.flag_raise(Flag::B);

        let ps = self.state.ps;
        self.writ_byte(sk_addr, ps);
        self.state.sp -= 1;

        self.flag_drop(Flag::U);
        self.flag_drop(Flag::B);
    }

    /// pull accumulator from stack
    pub fn pla(&mut self) {
        let sk_addr = STACK_BASE + self.state.sp as u16;
        self.state.a = self.read_bus(sk_addr);

        self.flag(Flag::Z, self.state.a == 0);
        self.flag(Flag::N, self.state.a & 0x80 != 0);
    }

    /// pull cpu status from stack
    pub fn plp(&mut self) {
        let sk_addr = STACK_BASE + self.state.sp as u16;
        self.state.ps = self.read_bus(sk_addr);
    }

    /// rotate one bit left
    /// memory or accumulator
    pub fn rol(&mut self) {
        let mut res: u16 = self.state.a as u16;
        res <<= 1;
        res |= self.flagv(Flag::C) as u16;

        self.flag(Flag::C, res > 0xff);
        self.flag(Flag::Z, res & 0x00ff == 0);
        self.flag(Flag::N, res & 0x80 != 0);

        if self.current.is_none() {
            error!("CURRENT IS EMPTY!");
        }

        let i = self.current.unwrap();

        if i.am == AddrMode::Implied || i.am == AddrMode::Accumulator {
            self.state.a = (res & 0x00ff) as u8;
        } else {
            let aa = self.addr_abs;
            let res_u8 = res as u8;
            self.writ_byte(aa, res_u8);
        }
    }

    /// rotate one bit right
    /// memory or accumulator
    pub fn ror(&mut self) {
        let mut res: u16 = self.state.a as u16;
        res >>= 1;
        res |= (self.flagv(Flag::C) as u16) << 7;

        self.flag(Flag::C, res & 0x1 != 0);
        self.flag(Flag::Z, res & 0x00ff == 0);
        self.flag(Flag::N, res & 0x80 != 0);

        if self.current.is_none() {
            error!("CURRENT IS EMPTY!");
        }

        let i = self.current.unwrap();

        if i.am == AddrMode::Implied || i.am == AddrMode::Accumulator {
            self.state.a = (res & 0x00ff) as u8;
        } else {
            let aa = self.addr_abs;
            let res_u8 = res as u8;
            self.writ_byte(aa, res_u8);
        }
    }

    /// return from interrupt
    /// pop cpu status and program counter
    pub fn rti(&mut self) {
        let mut sp = self.state.sp as u16;
        sp += 1;
        let mut addr_read = STACK_BASE + sp;
        let ps = self.read_bus(addr_read);

        addr_read += 1;
        let pc = self.read_word_and_inc(&mut addr_read);
        sp = addr_read - STACK_BASE - 1;

        self.state.ps = ps;
        self.state.sp = sp as u8;
        self.state.pc = pc;

        self.flag_drop(Flag::B);
        self.flag_drop(Flag::U);
    }

    /// return from subroutine
    /// pop pc
    pub fn rts(&mut self) {
        let mut sp = self.state.sp as u16;
        sp += 1;
        let mut addr_read = sp + STACK_BASE;
        let pc = self.read_word_and_inc(&mut addr_read);
        sp = addr_read - STACK_BASE - 1;

        self.state.pc = pc + 1;
        self.state.sp = sp as u8;
    }

    /// subtract memory from accumulator with borrow
    pub fn sbc(&mut self) {
        self.fetched = !self.fetched;
        self.adc();
    }

    /// set carry flag
    pub fn sec(&mut self) {
        self.flag_raise(Flag::C);
    }

    /// set decimal flag
    pub fn sed(&mut self) {
        self.flag_raise(Flag::D);
    }

    /// set interrupts disabled flag
    pub fn sei(&mut self) {
        self.flag_raise(Flag::I);
    }

    /// store accumulator in memory
    pub fn sta(&mut self) {
        let aa = self.addr_abs;
        let ac = self.state.a;
        self.writ_byte(aa, ac);
    }

    /// store x index in memory
    pub fn stx(&mut self) {
        let aa = self.addr_abs;
        let x = self.state.x;
        self.writ_byte(aa, x);
    }

    /// store y index in memory
    pub fn sty(&mut self) {
        let aa = self.addr_abs;
        let y = self.state.y;
        self.writ_byte(aa, y);
    }

    /// transfer accumulator in index x
    pub fn tax(&mut self) {
        self.state.x = self.state.a;
        let x = self.state.x;
        self.flag(Flag::N, x & 0x80 != 0);
        self.flag(Flag::Z, x == 0);
    }

    /// transfer accumulator in index y
    pub fn tay(&mut self) {
        self.state.y = self.state.a;
        let y = self.state.y;
        self.flag(Flag::N, y & 0x80 != 0);
        self.flag(Flag::Z, y == 0);
    }

    /// transfer stack pointer to index x
    pub fn tsx(&mut self) {
        self.state.x = self.state.sp;
        let x = self.state.x;
        self.flag(Flag::N, x & 0x80 != 0);
        self.flag(Flag::Z, x == 0);
    }

    /// transfer index x to accumulator
    pub fn txa(&mut self) {
        self.state.a = self.state.x;
        let a = self.state.a;
        self.flag(Flag::N, a & 0x80 != 0);
        self.flag(Flag::Z, a == 0);
    }

    /// transfer index x to stack pointer register
    pub fn txs(&mut self) {
        self.state.sp = self.state.x;
        let sp = self.state.sp;
        self.flag(Flag::N, sp & 0x80 != 0);
        self.flag(Flag::Z, sp == 0);
    }

    /// transfer index y to accumulator
    pub fn tya(&mut self) {
        self.state.a = self.state.y;
        let a = self.state.a;
        self.flag(Flag::N, a & 0x80 != 0);
        self.flag(Flag::Z, a == 0);
    }
}
