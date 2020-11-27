#![allow(dead_code)]

mod bus;
mod cpu;

#[macro_use]
extern crate maplit;
extern crate lazy_static;
extern crate log;

#[test]
fn it_works() {
    assert_eq!(1, 1);
}

// use cpu::mos6502::mos6502_addressing_modes::*;
use cpu::mos6502::*;

use bus::dummy_bus::*;

use chrono::Local;
use env_logger::Builder;
use log::LevelFilter;
use std::io::Write;

mod debugging {

    use super::*;

    pub(super) fn setup_logger() {
        log::set_max_level(LevelFilter::Info);
        Builder::new()
            .format(|buf, record| {
                writeln!(
                    buf,
                    "{} [{}] - {}",
                    Local::now().format("%Y-%m-%dT%H:%M:%S"),
                    record.level(),
                    record.args()
                )
            })
            .filter(None, LevelFilter::Info)
            .init();
    }
}

fn main() {
    debugging::setup_logger();
    let mut mos6502 = Cpu::new();

    let bus = mos6502.busline.as_mut().unwrap();
    let vec_prog = vec![
        0xA2, 0x0A, 0x8E, 0x00, 0x00, 0xA2, 0x03, 0x8E, 0x01, 0x00, 0xAC, 0x00, 0x00, 0xA9, 0x00,
        0x18, 0x6D, 0x01, 0x00, 0x88, 0xD0, 0xFA, 0x8D, 0x02, 0x00, 0xEA, 0xEA, 0xEA,
    ];
    let prog: &[u8] = &vec_prog[..];
    let load_addr: u16 = 0x200;
    bus.load_prog(&load_addr, prog);
    bus.dump();

    mos6502.reset();

    // let i = instr("Using ACC AM", 0x00, Cpu::rol, AddrMode::Accumulator, 1, 2);
    // mos6502.current = Some(i);
    // mos6502.tick();
}
