use olc_pixel_game_engine as olc;

use super::mos6502::*;

pub mod view {
    // use std::thread::sleep;
    // use std::time::Duration;

    use super::*;

    #[derive(PartialEq)]
    pub enum CpuViewPoint {
        MemView,
        CpuStateCodeView,
        DisassemblyView,
    }

    pub struct CpuView {
        pub cpu: Cpu,
        pub pov: CpuViewPoint,
        pub disassemble: Option<Asm>,
    }

    impl olc::Application for CpuView {
        fn on_user_create(&mut self) -> Result<(), olc::Error> {
            self.disassemble = Some(self.cpu.disassemble_region(0x0000, 0xffff));
            Ok(())
        }

        fn on_user_update(&mut self, _elapsed_time: f32) -> Result<(), olc::Error> {
            olc::clear(olc::BLACK);

            if olc::get_key(olc::Key::S).pressed {
                self.cpu.tick();
            }

            if olc::get_key(olc::Key::R).pressed {
                self.cpu.reset();
            }

            if olc::get_key(olc::Key::C).pressed {
                self.pov = CpuViewPoint::CpuStateCodeView;
            } else if olc::get_key(olc::Key::M).pressed {
                self.pov = CpuViewPoint::MemView;
            } else if olc::get_key(olc::Key::D).pressed {
                self.pov = CpuViewPoint::DisassemblyView;
            }

            if self.pov == CpuViewPoint::MemView {
                self.display_mem(50, 50, 0x0, 16, 16)?;
                self.display_mem(50, 270, 0x8000, 16, 16)?;
            } else if self.pov == CpuViewPoint::CpuStateCodeView {
                self.display_cpu_state(300, 200)?;
            } else if self.pov == CpuViewPoint::DisassemblyView {
                self.display_code(250, 50)?;
            }

            Ok(())
        }

        fn on_user_destroy(&mut self) -> Result<(), olc::Error> {
            Ok(())
        }
    }

    impl CpuView {
        pub fn display_mem(
            &self,
            x_begin: i32,
            y_begin: i32,
            addr_begin: u16,
            rows: u16,
            cols: u16,
        ) -> Result<(), olc::Error> {
            let mut disp_addr: String;
            let mut addr: u16 = addr_begin;
            let x: i32 = x_begin;
            let mut y: i32 = y_begin;
            for _i in 0..rows {
                disp_addr = format!("{:#06x}: ", addr);
                for _j in 0..cols {
                    let byte = self.cpu.read_and_inc(&mut addr);
                    disp_addr.push_str(format!(" {:#04x}", byte).as_str());
                }
                olc::draw_string(x, y, disp_addr.as_str(), olc::WHITE)?;
                y += 12;
                disp_addr.clear();
            }
            Ok(())
        }

        pub fn display_cpu_state(&self, x: i32, y: i32) -> Result<(), olc::Error> {
            use Flag::*;

            if let Some(i) = self.cpu.current {
                let mnemonic = i.mnemonic;
                olc::draw_string(x, y - 12, &format!("(at a {})", mnemonic), olc::WHITE)?;
            }

            let n = self.cpu.flagv(N);
            let v = self.cpu.flagv(V);
            let u = self.cpu.flagv(U);
            let b = self.cpu.flagv(B);
            let d = self.cpu.flagv(D);
            let i = self.cpu.flagv(I);
            let z = self.cpu.flagv(Z);
            let c = self.cpu.flagv(C);
            olc::draw_string(x, y, "P: ", olc::WHITE)?;
            olc::draw_string(x + 64, y, "N", if n { olc::GREY } else { olc::RED })?;
            olc::draw_string(x + 80, y, "V", if v { olc::GREEN } else { olc::RED })?;
            olc::draw_string(x + 96, y, "U", if u { olc::GREEN } else { olc::RED })?;
            olc::draw_string(x + 112, y, "B", if b { olc::GREEN } else { olc::RED })?;
            olc::draw_string(x + 128, y, "D", if d { olc::GREEN } else { olc::RED })?;
            olc::draw_string(x + 144, y, "I", if i { olc::GREEN } else { olc::RED })?;
            olc::draw_string(x + 160, y, "Z", if z { olc::GREEN } else { olc::RED })?;
            olc::draw_string(x + 176, y, "C", if c { olc::GREEN } else { olc::RED })?;

            olc::draw_string(
                x,
                y + 18,
                &format!("A={:#04x}", self.cpu.state.a),
                olc::WHITE,
            )?;
            olc::draw_string(
                x,
                y + 36,
                &format!("X={:#04x}", self.cpu.state.x),
                olc::WHITE,
            )?;
            olc::draw_string(
                x,
                y + 54,
                &format!("Y={:#04x}", self.cpu.state.y),
                olc::WHITE,
            )?;
            olc::draw_string(
                x,
                y + 72,
                &format!("SP={:#06x}", self.cpu.state.sp),
                olc::WHITE,
            )?;
            olc::draw_string(
                x,
                y + 90,
                &format!("PC={:#06x}", self.cpu.state.pc),
                olc::WHITE,
            )?;
            Ok(())
        }

        pub fn display_code(&self, x: i32, y: i32) -> Result<(), olc::Error> {
            if self.disassemble.is_none() {
                // Should it really return ok?
                return Ok(());
            }

            let disassembly = self.disassemble.as_ref().unwrap().this_map();
            let i = self.cpu.current;
            let pc = if i.is_none() {
                Vectors::RESET as u16
            } else {
                self.cpu.state.pc
            };
            const BACKWARDS: u16 = 20;
            const FORWARDS: u16 = 20;
            let begin = if pc.checked_sub(BACKWARDS).is_none() {
                0
            } else {
                pc - BACKWARDS
            };
            let end = if pc.checked_add(FORWARDS).is_none() {
                0xffff
            } else {
                pc + FORWARDS
            };
            let mut c: olc::Pixel;
            let mut this_y: i32;

            let mut count_displayed = 0;

            for addr in begin..end {
                if let Some(code) = disassembly.get(&addr) {
                    this_y = y + count_displayed * 12;
                    count_displayed += 1;
                    c = if addr == pc + 1 {
                        olc::fill_triangle(
                            x - 5,
                            this_y + 3,
                            x - 10,
                            this_y,
                            x - 10,
                            this_y + 7,
                            olc::GREEN,
                        );
                        olc::VERY_DARK_GREY
                    } else {
                        olc::WHITE
                    };
                    olc::draw_string(x, this_y, &format!("[{}]    {}", addr, code), c)?;
                }
            }

            Ok(())
        }
    }
}
