use olc_pixel_game_engine as olc;

use super::mos6502::*;

pub mod view {
    use std::thread::sleep;
    use std::time::Duration;

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
    }

    impl olc::Application for CpuView {
        fn on_user_create(&mut self) -> Result<(), olc::Error> {
            Ok(())
        }
        fn on_user_update(&mut self, _elapsed_time: f32) -> Result<(), olc::Error> {
            olc::clear(olc::BLACK);

            if olc::get_key(olc::Key::SPACE).pressed {
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
                self.display_cpu_state(200, 200)?;
            } else if self.pov == CpuViewPoint::DisassemblyView {
                self.display_code(50, 50)?;
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
            olc::draw_string(x, y, "P: ", olc::WHITE)?;
            olc::draw_string(
                x + 64,
                y,
                "N",
                if self.cpu.flag(Flag::N) {
                    olc::GREEN
                } else {
                    olc::RED
                },
            )?;
            olc::draw_string(
                x + 80,
                y,
                "V",
                if self.cpu.flag(Flag::V) {
                    olc::GREEN
                } else {
                    olc::RED
                },
            )?;
            olc::draw_string(
                x + 96,
                y,
                "U",
                if self.cpu.flag(Flag::U) {
                    olc::GREEN
                } else {
                    olc::RED
                },
            )?;
            olc::draw_string(
                x + 112,
                y,
                "B",
                if self.cpu.flag(Flag::B) {
                    olc::GREEN
                } else {
                    olc::RED
                },
            )?;
            olc::draw_string(
                x + 128,
                y,
                "I",
                if self.cpu.flag(Flag::I) {
                    olc::GREEN
                } else {
                    olc::RED
                },
            )?;
            olc::draw_string(
                x + 144,
                y,
                "Z",
                if self.cpu.flag(Flag::Z) {
                    olc::GREEN
                } else {
                    olc::RED
                },
            )?;
            olc::draw_string(
                x + 160,
                y,
                "C",
                if self.cpu.flag(Flag::C) {
                    olc::GREEN
                } else {
                    olc::RED
                },
            )?;
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
            // olc::draw_string(x, y, "code code code", olc::CYAN);
            let pc = self.cpu.state.pc;
            let begin = pc - 10;
            let end = pc + 10;
            let mut c = olc::WHITE;
            let mut this_y = y;
            // for tup in sorted {
            //     let l = tup.0;
            //     let code = tup.1;
            //     println!("{}\t{}", l, code);
            // }
            for (idx, tup) in self
                .cpu
                .disassemble_region(begin, end)
                .map_sorted()
                .iter()
                .enumerate()
            {
                let l = tup.0;
                let code = tup.1;
                c = if *l == pc { olc::DARK_CYAN } else { olc::WHITE };
                this_y = y + (idx as i32 * 12);
                olc::draw_string(x, this_y, &format!("{}\t{}", l, code), c)?;
            }
            Ok(())
        }
    }
}
