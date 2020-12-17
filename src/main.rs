#![feature(get_mut_unchecked)]
// #![allow(dead_code)]

extern crate lazy_static;
extern crate log;
extern crate log4rs;
#[macro_use]
extern crate maplit;
extern crate olc_pixel_game_engine;

use std::cell::RefCell;
use std::rc::Rc;

use olc_pixel_game_engine as olc;

use cart::cart::Cartridge;
use cpu::mos6502::*;
use nes::nes::Nes;
use vis::visuals::{GuiMonitor, MonitorPOV};

mod apu;
mod cart;
mod cpu;
mod nes;
mod ppu;
mod vis;

mod debugging {
    use super::*;

    pub(super) fn setup_logger() {
        log4rs::init_file("config/conf.yml", Default::default()).unwrap();
    }
}

fn main() {
    debugging::setup_logger();

    let mut nes_wrapper = Nes::new_wrapper();
    let nes: &mut Nes = unsafe { Rc::get_mut_unchecked(&mut nes_wrapper).get_mut() };
    nes.reset();

    let cart = Rc::new(RefCell::new(Cartridge::new("config/nestest.nes")));
    nes.insert_cart(cart.clone());

    let mos6502 = nes.cpu_mut();
    mos6502.state.pc = 0xc004;
    let disassemble = mos6502.disassemble_region(0x0000, 0xffff);
    let mut sorted: Vec<_> = disassemble.this_map().iter().collect();
    sorted.sort_by(|a, b| a.0.cmp(&b.0));

    let mut monitor = GuiMonitor {
        nes,
        pov: MonitorPOV::PictureView,
        disassemble: None,
        in_stepping_mode: false,
        extra_time: 0.,
    };

    olc::start("Nessy", &mut monitor, 256, 240, 1, 1).unwrap();
}
