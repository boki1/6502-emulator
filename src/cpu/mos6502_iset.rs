use super::mos6502::{mos6502_addressing_modes::AddrMode, Cpu, Flag, Vectors, STACK_BASE};

use log::*;
impl Cpu {
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

        self.state.a = (result & 0x00ff) as u8;
    }

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
            self.writ_byte(&aa, &res);
        }
    }

    // Executes the common behaviour of all BRANCH instructions
    fn __b_common(&mut self) {
        self.addr_abs = self.addr_rel + self.state.pc;
        self.additional_cycle = Cpu::same_page(self.addr_abs, self.state.pc);
        self.state.pc = self.addr_abs;
    }

    pub fn bcc(&mut self) {
        if self.flagv(Flag::C) == false {
            self.__b_common();
        }
    }

    pub fn bcs(&mut self) {
        if self.flagv(Flag::C) == true {
            self.__b_common();
        }
    }

    pub fn beq(&mut self) {
        if self.flagv(Flag::Z) == true {
            self.__b_common();
        }
    }

    pub fn bit(&mut self) {
        let conjuction = self.state.a & self.fetched;
        if conjuction == 0 {
            self.flag_raise(Flag::Z);
        }

        let n = self.fetched & 0x80 != 0;
        self.flag(Flag::N, n);

        let v = self.fetched & 0x40 != 0;
        self.flag(Flag::V, v);
    }

    pub fn bmi(&mut self) {
        if self.flagv(Flag::N) == true {
            self.__b_common();
        }
    }

    pub fn bne(&mut self) {
        if self.flagv(Flag::Z) == false {
            self.__b_common();
        }
    }

    pub fn bpl(&mut self) {
        if self.flagv(Flag::N) == false {
            self.__b_common();
        }
    }

    pub fn brk(&mut self) {
        let skptr_u16 = self.state.sp as u16;
        let mut pc_temp = self.state.pc;
        self.writ_word(&skptr_u16, &pc_temp);
        let p = self.flag_raise(Flag::B);
        self.writ_byte(&skptr_u16, &p);

        self.state.sp = skptr_u16 as u8;

        self.flag_drop(Flag::I);
        let mut load_addr = Vectors::IRQ as u16;
        let lo = self.read_and_inc(&mut load_addr) as u16;
        let hi = self.read_bus(&load_addr) as u16;
        pc_temp = hi << 8 | lo;

        self.state.pc = pc_temp;
    }

    pub fn bvc(&mut self) {
        if self.flagv(Flag::V) == false {
            self.__b_common();
        }
    }

    pub fn bvs(&mut self) {
        if self.flagv(Flag::V) == true {
            self.__b_common();
        }
    }

    pub fn clc(&mut self) {
        self.flag_drop(Flag::C);
    }

    pub fn cld(&mut self) {
        self.flag_drop(Flag::D);
    }

    pub fn cli(&mut self) {
        self.flag_drop(Flag::I);
    }

    pub fn clv(&mut self) {
        self.flag_drop(Flag::V);
    }

    fn __cmp_common(&mut self, reg: u8) {
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

    pub fn cmp(&mut self) {
        self.__cmp_common(self.state.a);
    }

    pub fn cpx(&mut self) {
        self.__cmp_common(self.state.x);
    }

    pub fn cpy(&mut self) {
        self.__cmp_common(self.state.y);
    }

    pub fn dec(&mut self) {
        let new_val = self.fetched - 1;
        let aa = self.addr_abs;
        self.writ_byte(&aa, &new_val);

        if new_val & 0x80 != 0 {
            self.flag_raise(Flag::N);
        }

        if new_val == 0 {
            self.flag_raise(Flag::Z);
        }
    }

    fn __inc_common(&mut self, reg_ref: &mut u8, val: u8, signed: bool) {
        if signed {
            *reg_ref -= val;
        } else {
            *reg_ref += val;
        }

        let r = *reg_ref;
        self.flag(Flag::N, r & 0x80 != 0);
        self.flag(Flag::Z, r == 0);
    }

    pub fn dex(&mut self) {
        let mut xx = self.state.x;
        self.__inc_common(&mut xx, 1, true);
        self.state.x = xx;
    }

    pub fn dey(&mut self) {
        let mut yy = self.state.y;
        self.__inc_common(&mut yy, 1, true);
        self.state.y = yy;
    }

    pub fn eor(&mut self) {
        self.state.a ^= self.fetched;
        self.flag(Flag::Z, self.state.a == 0);
        self.flag(Flag::N, self.state.a & 0x80 != 0);
    }

    pub fn inc(&mut self) {
        let new_val = self.fetched + 1;
        let aa = self.addr_abs;
        self.writ_byte(&aa, &new_val);

        self.flag(Flag::N, new_val & 0x80 != 0);
        self.flag(Flag::Z, new_val == 0);
    }

    pub fn inx(&mut self) {
        let mut xx = self.state.x;
        self.__inc_common(&mut xx, 1, false);
        self.state.x = xx;
    }

    pub fn iny(&mut self) {
        let mut yy = self.state.y;
        self.__inc_common(&mut yy, 1, false);
        self.state.y = yy;
    }

    pub fn jmp(&mut self) {
        self.state.pc = self.addr_abs;
    }

    pub fn jsr(&mut self) {
        self.state.pc -= 1;
        let sk_addr = STACK_BASE + self.state.sp as u16;
        let to_write = self.state.pc;
        self.writ_word(&sk_addr, &to_write);
        self.state.sp -= 2;

        self.state.pc = self.addr_abs;
    }

    fn __load_common(&mut self, reg_ref: &mut u8) {
        *reg_ref = self.fetched;
        self.flag(Flag::N, *reg_ref & 0x80 != 0);
        self.flag(Flag::Z, *reg_ref == 0);
    }

    pub fn lda(&mut self) {
        let mut ac = self.state.a;
        self.__load_common(&mut ac);
        self.state.a = ac;
    }

    pub fn ldx(&mut self) {
        let mut xx = self.state.x;
        self.__load_common(&mut xx);
        self.state.x = xx;
    }

    pub fn ldy(&mut self) {
        let mut yy = self.state.y;
        self.__load_common(&mut yy);
        self.state.y = yy;
    }

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
            self.writ_byte(&aa, &res);
        }
    }

    pub fn nop(&mut self) {
        if self.current.is_none() {
            error!("CURRENT IS EMPTY.");
        }

        let i = self.current.unwrap();

        // there is some difference in the nop (illegal opcodes stuff)
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

    pub fn ora(&mut self) {
        self.state.a |= self.fetched;

        self.flag(Flag::Z, self.state.a == 0);
        self.flag(Flag::N, self.state.a & 0x80 != 0x80);
    }

    pub fn pha(&mut self) {
        let sk_addr = STACK_BASE + self.state.sp as u16;
        let ac = self.state.a;
        self.writ_byte(&sk_addr, &ac);

        self.state.sp -= 1;
    }

    pub fn php(&mut self) {
        let sk_addr = STACK_BASE + self.state.sp as u16;
        self.flag_raise(Flag::U);
        self.flag_raise(Flag::B);

        let ps = self.state.ps;
        self.writ_byte(&sk_addr, &ps);
        self.state.sp -= 1;

        self.flag_drop(Flag::U);
        self.flag_drop(Flag::B);
    }

    pub fn pla(&mut self) {
        let sk_addr = STACK_BASE + self.state.sp as u16;
        self.state.a = self.read_bus(&sk_addr);

        self.flag(Flag::Z, self.state.a == 0);
        self.flag(Flag::N, self.state.a & 0x80 != 0);
    }

    pub fn plp(&mut self) {
        let sk_addr = STACK_BASE + self.state.sp as u16;
        self.state.ps = self.read_bus(&sk_addr);
    }

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
            self.writ_byte(&aa, &res_u8);
        }
    }

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
            self.writ_byte(&aa, &res_u8);
        }
    }

    pub fn rti(&mut self) {
        let mut sp = self.state.sp as u16;
        sp += 1;
        let mut addr_read = STACK_BASE + sp;
        let ps = self.read_bus(&addr_read);

        addr_read += 1;
        let pc = self.read_word_and_inc(&mut addr_read);
        sp = addr_read - STACK_BASE - 1;

        self.state.ps = ps;
        self.state.sp = sp as u8;
        self.state.pc = pc;

        self.flag_drop(Flag::B);
        self.flag_drop(Flag::U);
    }

    pub fn rts(&mut self) {
        let mut sp = self.state.sp as u16;
        sp += 1;
        let mut addr_read = sp + STACK_BASE;
        let pc = self.read_word_and_inc(&mut addr_read);
        sp = addr_read - STACK_BASE - 1;

        self.state.pc = pc + 1;
        self.state.sp = sp as u8;
    }

    pub fn sbc(&mut self) {
        self.fetched = !self.fetched;
        self.adc();
    }

    pub fn sec(&mut self) {
        self.flag_raise(Flag::C);
    }

    pub fn sed(&mut self) {
        self.flag_raise(Flag::D);
    }

    pub fn sei(&mut self) {
        self.flag_raise(Flag::I);
    }

    pub fn sta(&mut self) {
        let aa = self.addr_abs;
        let ac = self.state.a;
        self.writ_byte(&aa, &ac);
    }

    pub fn stx(&mut self) {
        let aa = self.addr_abs;
        let x = self.state.x;
        self.writ_byte(&aa, &x);
    }

    pub fn sty(&mut self) {
        let aa = self.addr_abs;
        let y = self.state.y;
        self.writ_byte(&aa, &y);
    }

    pub fn tax(&mut self) {
        self.state.x = self.state.a;
        let x = self.state.x;
        self.flag(Flag::N, x & 0x80 != 0);
        self.flag(Flag::Z, x == 0);
    }

    pub fn tay(&mut self) {
        self.state.y = self.state.a;
        let y = self.state.y;
        self.flag(Flag::N, y & 0x80 != 0);
        self.flag(Flag::Z, y == 0);
    }

    pub fn tsx(&mut self) {
        self.state.x = self.state.sp;
        let x = self.state.x;
        self.flag(Flag::N, x & 0x80 != 0);
        self.flag(Flag::Z, x == 0);
    }

    pub fn txa(&mut self) {
        self.state.a = self.state.x;
        let a = self.state.a;
        self.flag(Flag::N, a & 0x80 != 0);
        self.flag(Flag::Z, a == 0);
    }

    pub fn txs(&mut self) {
        self.state.sp = self.state.x;
        let sp = self.state.sp;
        self.flag(Flag::N, sp & 0x80 != 0);
        self.flag(Flag::Z, sp == 0);
    }

    pub fn tya(&mut self) {
        self.state.a = self.state.y;
        let a = self.state.a;
        self.flag(Flag::N, a & 0x80 != 0);
        self.flag(Flag::Z, a == 0);
    }
}
