use olc_pixel_game_engine as olc;

use crate::cpu::mos6502::{Asm, Flag::*, Vectors};
use crate::nes::nes::Nes;
use crate::ppu::ppu_2c02::PpuReg;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Layouts {
    RAM,
    Internals,
    Disassembly,
    PaletteAndPattern,
    Help,
    Game,
}

impl Layouts {
    fn to_string(&self) -> String {
        use Layouts::*;

        let rv = match *self {
            RAM => "RAM",
            Internals => "NES Internals",
            Disassembly => "Disassembly",
            PaletteAndPattern => "PPU palette and pattern",
            Help => "Help",
            Game => "Game",
        };

        rv.to_string()
    }
}

pub struct GuiMonitor<'console> {
    pub nes: &'console mut Nes,
    pub curr_layout: Layouts,
    pub disassembly: Option<Asm>,
    pub in_stepping_mode: bool,
    pub extra_time: f32,
}

impl olc::Application for GuiMonitor<'_> {
    fn on_user_create(&mut self) -> Result<(), olc::Error> {
        let cpu = self.nes.cpu_mut();

        self.disassembly = Some(cpu.disassemble_region(0x0000, 0xffff));
        Ok(())
    }

    fn on_user_update(&mut self, _elapsed_time: f32) -> Result<(), olc::Error> {
        use olc::{get_key, Key::*};

        olc::clear(olc::BLACK);

        self.handle_layout_input();

        if self.in_stepping_mode {
            self.handle_debugger_input();
        } else {
            self.nes.sys_clock();
        }

        self.display_curr_layout()?;
        // self.handle_controller_input();

        if get_key(SPACE).pressed {
            self.in_stepping_mode = !self.in_stepping_mode;
        }

        if get_key(R).pressed {
            self.nes.reset();
        }

        Ok(())
    }

    fn on_user_destroy(&mut self) -> Result<(), olc::Error> {
        Ok(())
    }
}

impl GuiMonitor<'_> {
    fn display_layout_meta_info(&mut self) -> Result<(), olc::Error> {
        olc::draw_string(2, 2, &self.curr_layout.to_string(), olc::WHITE)?;

        if self.in_stepping_mode {
            olc::fill_rect(700, 2, 8, 20, olc::RED);
            olc::fill_rect(715, 2, 8, 20, olc::RED);
        } else {
            olc::fill_triangle(700, 2, 700, 22, 715, 12, olc::GREEN);
        }

        Ok(())
    }

    fn display_curr_layout(&mut self) -> Result<(), olc::Error> {
        use Layouts::*;
        self.display_layout_meta_info()?;

        match self.curr_layout {
            RAM => {
                self.display_mem(50, 50, 0x0, 16, 16)?;
                self.display_mem(50, 270, 0x8000, 16, 16)?;
            }
            Internals => {
                self.display_cpu_state(20, 20)?;
                self.display_ppu_state(60, 20)?;
                self.display_controller_state(100, 20)?;
            }
            PaletteAndPattern => {
                self.display_pattern_with_palette(100, 100)?;
            }
            Disassembly => {
                self.display_code(50, 50)?;
            }
            Game => {
                self.display_game(136, 90);
            }
            Help => {}
        };

        Ok(())
    }

    fn handle_layout_input(&mut self) {
        use olc::{get_key, Key::*};
        use Layouts::*;

        self.curr_layout = if get_key(G).pressed {
            Game
        } else if get_key(I).pressed {
            Internals
        } else if get_key(M).pressed {
            RAM
        } else if get_key(D).pressed {
            Disassembly
        } else if get_key(T).pressed {
            PaletteAndPattern
        } else if get_key(H).pressed {
            Help
        } else {
            self.curr_layout
        }
    }

    fn handle_debugger_input(&mut self) {
        use olc::{get_key, Key::*};

        if get_key(N).pressed {
            self.nes.sys_clock();
        }

        if get_key(F).pressed {
            while self.nes.ppu().frame_has_ended() == false {
                self.nes.sys_clock();
            }
            self.nes.ppu.frame_end = false;
        }

        if get_key(S).pressed {
            while !self.nes.cpu().instr_has_executed() {
                self.nes.sys_clock();
            }
            while self.nes.cpu().instr_has_executed() {
                self.nes.sys_clock();
            }
        }
    }

    fn handle_controller_input(&mut self) {
        // TODO:
        unimplemented!();
    }
}

impl GuiMonitor<'_> {
    pub fn display_game(&self, x_begin: i32, y_begin: i32) {
        olc::draw_sprite_with_scale_and_flip(
            x_begin,
            y_begin,
            self.nes.ppu.screen(),
            2,
            olc::SpriteFlip::NONE,
        );
    }

    pub fn display_mem(
        &mut self,
        x_begin: i32,
        y_begin: i32,
        addr_begin: u16,
        rows: u16,
        cols: u16,
    ) -> Result<(), olc::Error> {
        let cpu = self.nes.cpu_mut();
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

    pub fn display_pattern_with_palette(&self, x: i32, y: i32) -> Result<(), olc::Error> {
        const PALETTE_LIMIT: i32 = 8;
        let chosen: i32 = 3;
        for palette_idx in 0..PALETTE_LIMIT {
            for idx in 0..4 {
                olc::fill_rect(
                    50 + x + palette_idx * chosen * 15 + chosen * (idx + 10),
                    300 + y,
                    chosen * 10,
                    chosen * 10,
                    self.nes
                        .ppu()
                        .map_pixel_to_color(palette_idx as u16, idx as u8),
                );
            }
        }

        olc::draw_sprite_with_scale_and_flip(
            x,
            y + 30,
            &self.nes.ppu().get_drawable_pattern(0, chosen as u16),
            2,
            olc::SpriteFlip::NONE,
        );
        olc::draw_sprite_with_scale_and_flip(
            x + 270,
            y + 30,
            &self.nes.ppu().get_drawable_pattern(1, chosen as u16),
            2,
            olc::SpriteFlip::NONE,
        );

        Ok(())
    }

    pub fn display_controller_state(&self, x: i32, y: i32) -> Result<(), olc::Error> {
        // TODO:
        // unimplemented!();
        Ok(())
    }

    pub fn display_ppu_state(&self, x: i32, y: i32) -> Result<(), olc::Error> {
        let ppu = self.nes.ppu();
        let ctrl = ppu.reg_set.control_reg.get();
        let mask = ppu.reg_set.mask_reg.get();
        let status = ppu.reg_set.status_reg.get();
        let oam_addr = 0u8;
        let oam_data = 0u8;
        let scroll = 0u8;
        let addr = 0u8;
        let data = 0u8;
        let dma = 0u8;

        let loopy_t: u16 = ppu.reg_set.t_addr.get();
        let loopy_v: u16 = ppu.reg_set.v_addr.get();
        let (scanlines, cycles) = (ppu.reg_set.dot.scanline(), ppu.reg_set.dot.cycles());

        let ppu_x = x + 300;
        let ppu_y = y + 50;

        olc::draw_string_with_scale(ppu_x - 10, ppu_y - 10, "NTSC 2C02", olc::DARK_MAGENTA, 3)?;
        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 25,
            &format!("PPUCTRL={:#02x}", ctrl),
            olc::DARK_CYAN,
            2,
        )?;
        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 50,
            &format!("PPUMASK={:#02x}", mask),
            olc::DARK_CYAN,
            2,
        )?;
        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 75,
            &format!("PPUSTATUS={:#02x}", status),
            olc::DARK_CYAN,
            2,
        )?;

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 100,
            &format!("OAMADDR={:#02x}", oam_addr),
            olc::DARK_CYAN,
            2,
        )?;

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 125,
            &format!("OAMDATA={:#02x}", oam_data),
            olc::DARK_CYAN,
            2,
        )?;

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 150,
            &format!("PPUSCROLL={:#02x}", scroll),
            olc::DARK_CYAN,
            2,
        )?;

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 175,
            &format!("PPUADDR={:#02x}", addr),
            olc::DARK_CYAN,
            2,
        )?;

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 200,
            &format!("PPUDATA={:#02x}", data),
            olc::DARK_CYAN,
            2,
        )?;

        olc::draw_line(
            ppu_x - 30,
            ppu_y + 225,
            ppu_x + 200,
            ppu_y + 225,
            olc::WHITE,
        );

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 230,
            &format!("DMA={:#02x}", dma),
            olc::DARK_GREEN,
            2,
        )?;

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 255,
            &format!("LOOPY_T={:#04x}", loopy_t),
            olc::DARK_GREEN,
            2,
        )?;

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 280,
            &format!("LOOPY_V={:#04x}", loopy_v),
            olc::DARK_GREEN,
            2,
        )?;

        olc::draw_line(
            ppu_x - 30,
            ppu_y + 305,
            ppu_x + 200,
            ppu_y + 305,
            olc::WHITE,
        );

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 310,
            &format!("SCANLINE={}", scanlines),
            olc::DARK_YELLOW,
            2,
        )?;

        olc::draw_string_with_scale(
            ppu_x,
            ppu_y + 335,
            &format!("CYCLES={}", cycles),
            olc::DARK_YELLOW,
            2,
        )?;

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

        let cpu_x = x + 30;
        let cpu_y = y + 50;

        olc::draw_string_with_scale(cpu_x - 10, cpu_y - 10, "MOS 6502", olc::DARK_MAGENTA, 3)?;

        olc::draw_string_with_scale(
            cpu_x,
            cpu_y + 25,
            &format!("A={:#04x}", cpu.state.a),
            olc::GREY,
            2,
        )?;
        olc::draw_string_with_scale(
            cpu_x,
            cpu_y + 50,
            &format!("X={:#04x}", cpu.state.x),
            olc::GREY,
            2,
        )?;
        olc::draw_string_with_scale(
            cpu_x,
            cpu_y + 75,
            &format!("Y={:#04x}", cpu.state.y),
            olc::GREY,
            2,
        )?;
        olc::draw_string_with_scale(
            cpu_x,
            cpu_y + 100,
            &format!("SP={:#06x}", cpu.state.sp),
            olc::GREY,
            2,
        )?;
        olc::draw_string_with_scale(
            cpu_x,
            cpu_y + 125,
            &format!("PC={:#06x}", cpu.state.pc),
            olc::GREY,
            2,
        )?;

        olc::draw_line(cpu_x, cpu_y + 150, cpu_x + 200, cpu_y + 150, olc::WHITE);

        olc::draw_string_with_scale(cpu_x, cpu_y + 165, "N", olc::WHITE, 2)?;
        olc::fill_rect(
            cpu_x,
            cpu_y + 182,
            14,
            4,
            if n { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(cpu_x + 15, cpu_y + 165, "V", olc::WHITE, 2)?;
        olc::fill_rect(
            cpu_x + 15,
            cpu_y + 182,
            14,
            4,
            if v { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(cpu_x + 30, cpu_y + 165, "U", olc::WHITE, 2)?;
        olc::fill_rect(
            cpu_x + 30,
            cpu_y + 182,
            14,
            4,
            if u { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(cpu_x + 45, cpu_y + 165, "B", olc::WHITE, 2)?;
        olc::fill_rect(
            cpu_x + 45,
            cpu_y + 182,
            14,
            4,
            if b { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(cpu_x + 60, cpu_y + 165, "D", olc::WHITE, 2)?;
        olc::fill_rect(
            cpu_x + 60,
            cpu_y + 182,
            14,
            4,
            if d { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(cpu_x + 75, cpu_y + 165, "I", olc::WHITE, 2)?;
        olc::fill_rect(
            cpu_x + 75,
            cpu_y + 182,
            14,
            4,
            if i { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(cpu_x + 90, cpu_y + 165, "Z", olc::WHITE, 2)?;
        olc::fill_rect(
            cpu_x + 90,
            cpu_y + 182,
            14,
            4,
            if z { olc::GREEN } else { olc::RED },
        );

        olc::draw_string_with_scale(cpu_x + 105, cpu_y + 165, "C", olc::WHITE, 2)?;
        olc::fill_rect(
            cpu_x + 105,
            cpu_y + 182,
            14,
            4,
            if c { olc::GREEN } else { olc::RED },
        );

        Ok(())
    }

    pub fn display_code(&self, x: i32, y: i32) -> Result<(), olc::Error> {
        if self.disassembly.is_none() {
            olc::draw_string_with_scale(x, y + 200, "No disassembly :(", olc::DARK_RED, 3)?;
            return Ok(());
        }

        let cpu = self.nes.cpu();

        let disassembly = self.disassembly.as_ref().unwrap().this_map();
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
