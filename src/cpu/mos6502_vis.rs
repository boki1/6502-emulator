use super::mos6502::*;
use olc_pixel_game_engine as olc;

pub mod view {
    use super::*;

    #[derive(PartialEq)]
    pub enum CpuViewPoint {
        MemView,
        CpuStateCodeView,
        CombinedView,
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
            olc::clear(olc::BLUE);

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
            }

            if self.pov == CpuViewPoint::MemView {
                self.display_mem(50, 50, 0x0, 16, 16)?;
                self.display_mem(50, 270, 0x8000, 16, 16)?;
            } else if self.pov == CpuViewPoint::CpuStateCodeView {
                olc::draw_rect(130, 175, 200, 150, olc::WHITE);
                olc::draw_rect(110, 155, 240, 190, olc::WHITE);
                self.display_cpu_state(200, 200)?;
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
            Ok(())
        }
    }
}
