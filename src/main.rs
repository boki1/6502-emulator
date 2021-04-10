#![allow(dead_code)]

extern crate ansi_term;
extern crate iced;
extern crate lazy_static;
extern crate log;
extern crate log4rs;
#[macro_use]
extern crate maplit;

// use bus::dummy_bus::*;
use cpu::mos6502::*;
use cpu::mos6502_vis::view::*;
// use std::collections::HashMap;
// use std::iter::FromIterator;

use iced::{Application, Settings};

use cpu::mos6502_vis::view::NessyGui;

mod bus;
mod cpu;

mod debugging {
    use super::*;

    pub(super) fn setup_logger() {
        log4rs::init_file("config/conf.yml", Default::default()).unwrap();
    }
}

fn main() -> iced::Result {
    debugging::setup_logger();

    NessyGui::run(Settings::default())
}
