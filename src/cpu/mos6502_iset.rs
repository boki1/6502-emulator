
use log::*;
use super::mos6502::Cpu;
    impl Cpu {
	pub fn adc(&mut self) {
		info!("adc-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn and(&mut self) {
		info!("and-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn asl(&mut self) {
		info!("asl-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn bcc(&mut self) {
		info!("bcc-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn bcs(&mut self) {
		info!("bcs-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn beq(&mut self) {
		info!("beq-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn bit(&mut self) {
		info!("bit-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn bmi(&mut self) {
		info!("bmi-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn bne(&mut self) {
		info!("bne-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn bpl(&mut self) {
		info!("bpl-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn brk(&mut self) {
		info!("brk-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn bvc(&mut self) {
		info!("bvc-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn bvs(&mut self) {
		info!("bvs-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn clc(&mut self) {
		info!("clc-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn cld(&mut self) {
		info!("cld-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn cli(&mut self) {
		info!("cli-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn clv(&mut self) {
		info!("clv-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn cmp(&mut self) {
		info!("cmp-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn cpx(&mut self) {
		info!("cpx-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn cpy(&mut self) {
		info!("cpy-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn dec(&mut self) {
		info!("dec-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn dex(&mut self) {
		info!("dex-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn dey(&mut self) {
		info!("dey-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn eor(&mut self) {
		info!("eor-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn inc(&mut self) {
		info!("inc-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn inx(&mut self) {
		info!("inx-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn iny(&mut self) {
		info!("iny-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn jmp(&mut self) {
		info!("jmp-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn jsr(&mut self) {
		info!("jsr-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn lda(&mut self) {
		info!("lda-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn ldx(&mut self) {
		info!("ldx-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn ldy(&mut self) {
		info!("ldy-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn lsr(&mut self) {
		info!("lsr-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn nop(&mut self) {
		info!("nop-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn ora(&mut self) {
		info!("ora-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn pha(&mut self) {
		info!("pha-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn php(&mut self) {
		info!("php-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn pla(&mut self) {
		info!("pla-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn plp(&mut self) {
		info!("plp-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn rol(&mut self) {
		info!("rol-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn ror(&mut self) {
		info!("ror-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn rti(&mut self) {
		info!("rti-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn rts(&mut self) {
		info!("rts-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn sbc(&mut self) {
		info!("sbc-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn sec(&mut self) {
		info!("sec-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn sed(&mut self) {
		info!("sed-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn sei(&mut self) {
		info!("sei-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn sta(&mut self) {
		info!("sta-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn stx(&mut self) {
		info!("stx-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn sty(&mut self) {
		info!("sty-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn tax(&mut self) {
		info!("tax-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn tay(&mut self) {
		info!("tay-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn tsx(&mut self) {
		info!("tsx-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn txa(&mut self) {
		info!("txa-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn txs(&mut self) {
		info!("txs-ing...");
		info!("{:?}", self.fetched);
	}

	pub fn tya(&mut self) {
		info!("tya-ing...");
		info!("{:?}", self.fetched);
	}

}