#![allow(dead_code)]

mod bus;
mod cpu;

#[macro_use]
extern crate maplit;
extern crate ansi_term;
extern crate lazy_static;
extern crate log;
extern crate log4rs;
extern crate olc_pixel_game_engine;

use olc_pixel_game_engine as olc;

use bus::dummy_bus::*;
use cpu::mos6502::*;
use cpu::mos6502_vis::view;

mod debugging {

    use super::*;
    pub(super) fn setup_logger() {
        log4rs::init_file("config/conf.yml", Default::default()).unwrap();
    }
}

fn main() {
    debugging::setup_logger();

    let mut mos6502 = Cpu::new();
    let load_addr: u16 = 0x8000;
    let pc_initial = Vectors::RESET as u16;
    mos6502.writ_word(&pc_initial, &load_addr);

    mos6502.reset();

    let bus = mos6502.busline.as_mut().unwrap();
    let prog: &[u8] = &vec![
        0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03, 0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9, 0x00,
        0x18, 0x6D, 0x01, 0x00, 0x88, 0xD0, 0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA,
    ];

    bus.load_prog(&load_addr, prog);
    let ibus = &*bus;
    ibus.dump(Some("memdump.txt".to_string()));

    let mut cpu_window = view::CpuView { cpu: mos6502 };
    olc::start("mos 6502", &mut cpu_window, 800, 620, 3, 3).unwrap();

    // loop {
    //     mos6502.tick();
    //     if mos6502.current.is_none() {
    //         break;
    //     }
    //
    //     if mos6502.current.unwrap().opcode == 0x00 {
    //         break;
    //     }
    // }

    // let asm_code = mos6502.disasemble_region(0x8000, 0x8020);
    // mos6502.disasemble_region(0x8000, 0x801d);
    // println!("here's the asm code\n");
    // println!("done.");

    // let mut _inp: String = String::new();
    // std::io::stdin().read_line(&mut _inp).unwrap();
    // std::fs::remove_file("output.log").unwrap();
}
