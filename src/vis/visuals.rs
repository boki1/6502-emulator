use olc_pixel_game_engine as olc;

use crate::cpu::mos6502::{Asm, Flag::*, Vectors};
use crate::nes::nes::Nes;

#[derive(PartialEq)]
pub enum MonitorPOV {
    MemView,
    CpuStateCodeView,
    DisassemblyView,
    PictureView,
}

pub struct GuiMonitor<'console> {
    pub nes: &'console mut Nes,
    pub pov: MonitorPOV,
    pub disassemble: Option<Asm>,
    pub in_stepping_mode: bool,
    pub extra_time: f32,
}

impl olc::Application for GuiMonitor<'_> {
    fn on_user_create(&mut self) -> Result<(), olc::Error> {
        let cpu = self.nes.cpu_mut();

        self.disassemble = Some(cpu.disassemble_region(0x0000, 0xffff));
        Ok(())
    }

    fn on_user_update(&mut self, elapsed_time: f32) -> Result<(), olc::Error> {
        olc::clear(olc::BLACK);

        if self.in_stepping_mode {
            self.check_input();
            self.check_pov_input();
        } else {
            self.nes.sys_clock();
        }

        self.display();

        if olc::get_key(olc::Key::T).pressed {
            self.in_stepping_mode = !self.in_stepping_mode;
        }

        Ok(())
    }

    fn on_user_destroy(&mut self) -> Result<(), olc::Error> {
        Ok(())
    }
}

impl GuiMonitor<'_> {
    fn display(&self) {
        match self.pov {
            MonitorPOV::MemView => {
                self.display_mem(50, 50, 0x0, 16, 16);
                self.display_mem(50, 270, 0x8000, 16, 16);
            }
            MonitorPOV::CpuStateCodeView => {
                self.display_cpu_state(50, 50);
            }
            MonitorPOV::DisassemblyView => {
                self.display_code(150, 50);
            }
            MonitorPOV::PictureView => {
                self.display_picture(2, 2);
            }
        }
    }

    fn check_pov_input(&mut self) {
        if olc::get_key(olc::Key::P).pressed {
            self.pov = MonitorPOV::PictureView;
        } else if olc::get_key(olc::Key::C).pressed {
            self.pov = MonitorPOV::CpuStateCodeView;
        } else if olc::get_key(olc::Key::M).pressed {
            self.pov = MonitorPOV::MemView;
        } else if olc::get_key(olc::Key::D).pressed {
            self.pov = MonitorPOV::DisassemblyView;
        }
    }

    fn check_input(&mut self) {
        let cpu = self.nes.cpu_mut();

        if olc::get_key(olc::Key::S).pressed {
            cpu.tick();
        }

        if olc::get_key(olc::Key::N).pressed {
            self.nes.sys_clock();
        }

        if olc::get_key(olc::Key::F).pressed {
            while self.nes.ppu.frame_end == false {
                self.nes.sys_clock();
            }
            self.nes.ppu.frame_end = false;
        }

        if olc::get_key(olc::Key::R).pressed {
            self.nes.reset();
        }
    }
}

impl GuiMonitor<'_> {
    pub fn display_picture(&self, x_begin: i32, y_begin: i32) {
        olc::draw_sprite(x_begin, y_begin, self.nes.ppu.screen());
    }

    pub fn display_mem(
        &self,
        x_begin: i32,
        y_begin: i32,
        addr_begin: u16,
        rows: u16,
        cols: u16,
    ) -> Result<(), olc::Error> {
        let cpu = self.nes.cpu();
        let mut disp_addr: String;
        let mut addr: u16 = addr_begin;
        let x: i32 = x_begin;
        let mut y: i32 = y_begin;
        for _i in 0..rows {
            disp_addr = format!("{:#06x}: ", addr);
            for _j in 0..cols {
                let byte = cpu.read_and_inc(&mut addr);
                disp_addr.push_str(format!(" {:#04x}", byte).as_str());
            }
            olc::draw_string(x, y, disp_addr.as_str(), olc::WHITE)?;
            y += 12;
            disp_addr.clear();
        }
        Ok(())
    }

    pub fn display_cpu_state(&self, x: i32, y: i32) -> Result<(), olc::Error> {
        let cpu = self.nes.cpu();
        let n = cpu.flagv(N);
        let v = cpu.flagv(V);
        let u = cpu.flagv(U);
        let b = cpu.flagv(B);
        let d = cpu.flagv(D);
        let i = cpu.flagv(I);
        let z = cpu.flagv(Z);
        let c = cpu.flagv(C);

        olc::draw_string_with_scale(x, y - 50, "MOS 6502", olc::WHITE, 3)?;

        olc::draw_string_with_scale(x, y + 25, &format!("A={:#04x}", cpu.state.a), olc::WHITE, 2)?;
        olc::draw_string_with_scale(x, y + 50, &format!("X={:#04x}", cpu.state.x), olc::WHITE, 2)?;
        olc::draw_string_with_scale(x, y + 75, &format!("Y={:#04x}", cpu.state.y), olc::WHITE, 2)?;
        olc::draw_string_with_scale(
            x,
            y + 100,
            &format!("SP={:#06x}", cpu.state.sp),
            olc::WHITE,
            2,
        )?;
        olc::draw_string_with_scale(
            x,
            y + 125,
            &format!("PC={:#06x}", cpu.state.pc),
            olc::WHITE,
            2,
        )?;

        olc::draw_line(x, y + 150, x + 200, y + 150, olc::WHITE);

        olc::draw_string_with_scale(x, y + 165, "N", olc::WHITE, 2)?;
        olc::fill_rect(x, y + 182, 14, 4, if n { olc::GREEN } else { olc::RED });

        olc::draw_string_with_scale(x + 15, y + 165, "V", olc::WHITE, 2)?;
        olc::fill_rect(
            x + 15,
            y + 182,
            14,
            4,
            if v { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(x + 30, y + 165, "U", olc::WHITE, 2)?;
        olc::fill_rect(
            x + 30,
            y + 182,
            14,
            4,
            if u { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(x + 45, y + 165, "B", olc::WHITE, 2)?;
        olc::fill_rect(
            x + 45,
            y + 182,
            14,
            4,
            if b { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(x + 60, y + 165, "D", olc::WHITE, 2)?;
        olc::fill_rect(
            x + 60,
            y + 182,
            14,
            4,
            if d { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(x + 75, y + 165, "I", olc::WHITE, 2)?;
        olc::fill_rect(
            x + 75,
            y + 182,
            14,
            4,
            if i { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(x + 90, y + 165, "Z", olc::WHITE, 2)?;
        olc::fill_rect(
            x + 90,
            y + 182,
            14,
            4,
            if z { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(x + 105, y + 165, "C", olc::WHITE, 2)?;
        olc::fill_rect(
            x + 105,
            y + 182,
            14,
            4,
            if c { olc::GREEN } else { olc::RED },
        );

        Ok(())
    }

    pub fn display_code(&self, x: i32, y: i32) -> Result<(), olc::Error> {
        if self.disassemble.is_none() {
            olc::draw_string_with_scale(x, y + 200, "No disassembly :(", olc::DARK_RED, 3)?;
            return Ok(());
        }

        let cpu = self.nes.cpu();

        let disassembly = self.disassemble.as_ref().unwrap().this_map();
        let i = cpu.current;
        let pc = if i.is_none() {
            Vectors::RESET as u16
        } else {
            cpu.state.pc
        };
        const BACKWARDS: u16 = 10;
        const FORWARDS: u16 = 30;
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
                this_y = y + count_displayed * 25;
                count_displayed += 1;
                if addr == pc + 1 {
                    olc::fill_triangle(
                        x - 5,
                        this_y,
                        x - 10,
                        this_y - 3,
                        x - 10,
                        this_y + 3,
                        olc::GREEN,
                    );

                    olc::draw_line(x - 25, this_y + 15, x + 500, this_y + 15, olc::GREEN);
                }
                olc::draw_string(x, this_y, &format!("[{}]    {}", addr, code), olc::WHITE)?;
            }
        }

        Ok(())
    }
}
