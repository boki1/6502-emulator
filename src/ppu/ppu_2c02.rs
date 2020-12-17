use std::cell::RefCell;
use std::rc::Rc;
// use rand;

use olc_pixel_game_engine::{Pixel, Sprite};

use crate::cart::cart::Cartridge;
use crate::nes::nes::{Nes, NesComponent};

const HORIZONTAL_LIMIT: i32 = 340;
const VERTICAL_LIMIT: i32 = 260;

pub const WIDTH: i32 = 256;
pub const HEIGHT: i32 = 240;

/// The Picture processing unit
pub struct Ppu {
    container: Option<Rc<RefCell<Nes>>>,
    cart: Option<Rc<Cartridge>>,
    pattern_mem: [u8; 8 * 1024],
    vram: [u8; 2 * 1024],
    palette_mem: [u8; 32],

    colours: [Pixel; 64],
    screen: Sprite,

    col: i32,
    row: i32,

    pub frame_end: bool,
}

impl std::fmt::Debug for Ppu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ppu")
            .field("row", &self.row)
            .field("col", &self.col)
            .finish()
    }
}

impl NesComponent for Ppu {
    type NesConsole = Nes;
    type NesConsolePtr = Rc<RefCell<Self::NesConsole>>;

    fn attach_to(&mut self, container: Self::NesConsolePtr) {
        self.container = Some(container);
    }

    fn container_of(&self) -> &Self::NesConsole {
        unsafe { self.container.as_ref().unwrap().as_ptr().as_ref().unwrap() }
    }

    fn container_of_mut(&mut self) -> &mut Self::NesConsole {
        unsafe { self.container.as_mut().unwrap().as_ptr().as_mut().unwrap() }
    }
}

impl Ppu {
    pub fn new(cart: Option<Rc<Cartridge>>) -> Self {
        Ppu {
            container: None,
            cart,
            pattern_mem: [0; 8 * 1024],
            vram: [0; 2 * 1024],
            palette_mem: [0; 32],
            screen: Sprite::with_dims(WIDTH, HEIGHT),
            col: 0,
            row: 0,
            frame_end: false,
            colours: [
                Pixel::rgb(84, 84, 84),
                Pixel::rgb(0, 30, 116),
                Pixel::rgb(8, 16, 144),
                Pixel::rgb(48, 0, 136),
                Pixel::rgb(68, 0, 100),
                Pixel::rgb(92, 0, 48),
                Pixel::rgb(84, 4, 0),
                Pixel::rgb(60, 24, 0),
                Pixel::rgb(32, 42, 0),
                Pixel::rgb(8, 58, 0),
                Pixel::rgb(0, 64, 0),
                Pixel::rgb(0, 60, 0),
                Pixel::rgb(0, 50, 60),
                Pixel::rgb(0, 0, 0),
                Pixel::rgb(0, 0, 0),
                Pixel::rgb(0, 0, 0),
                Pixel::rgb(152, 150, 152),
                Pixel::rgb(8, 76, 196),
                Pixel::rgb(48, 50, 236),
                Pixel::rgb(92, 30, 228),
                Pixel::rgb(136, 20, 176),
                Pixel::rgb(160, 20, 100),
                Pixel::rgb(152, 34, 32),
                Pixel::rgb(120, 60, 0),
                Pixel::rgb(84, 90, 0),
                Pixel::rgb(40, 114, 0),
                Pixel::rgb(8, 124, 0),
                Pixel::rgb(0, 118, 40),
                Pixel::rgb(0, 102, 120),
                Pixel::rgb(0, 0, 0),
                Pixel::rgb(0, 0, 0),
                Pixel::rgb(0, 0, 0),
                Pixel::rgb(236, 238, 236),
                Pixel::rgb(76, 154, 236),
                Pixel::rgb(120, 124, 236),
                Pixel::rgb(176, 98, 236),
                Pixel::rgb(228, 84, 236),
                Pixel::rgb(236, 88, 180),
                Pixel::rgb(236, 106, 100),
                Pixel::rgb(212, 136, 32),
                Pixel::rgb(160, 170, 0),
                Pixel::rgb(116, 196, 0),
                Pixel::rgb(76, 208, 32),
                Pixel::rgb(56, 204, 108),
                Pixel::rgb(56, 180, 204),
                Pixel::rgb(60, 60, 60),
                Pixel::rgb(0, 0, 0),
                Pixel::rgb(0, 0, 0),
                Pixel::rgb(236, 238, 236),
                Pixel::rgb(168, 204, 236),
                Pixel::rgb(188, 188, 236),
                Pixel::rgb(212, 178, 236),
                Pixel::rgb(236, 174, 236),
                Pixel::rgb(236, 174, 212),
                Pixel::rgb(236, 180, 176),
                Pixel::rgb(228, 196, 144),
                Pixel::rgb(204, 210, 120),
                Pixel::rgb(180, 222, 120),
                Pixel::rgb(168, 226, 144),
                Pixel::rgb(152, 226, 180),
                Pixel::rgb(160, 214, 228),
                Pixel::rgb(160, 162, 160),
                Pixel::rgb(0, 0, 0),
                Pixel::rgb(0, 0, 0),
            ],
        }
    }

    /// Write to PPU/secondary bus
    pub fn write(&mut self, addr: u16, _val: u8) {
        let _valid_addr = addr & 0x3fff;
    }

    /// Read from PPU/secondary bus
    pub fn read(&self, addr: u16) -> u8 {
        let _valid_addr = addr & 0x3fff;
        let data = 0;

        data
    }

    /// Write to main bus
    pub fn poke_main(&mut self, _addr: u16, _val: u8) {}

    /// Read from main bus
    pub fn peek_main(&mut self, _addr: u16) -> u8 {
        0
    }

    pub fn clock(&mut self) {
        let noise = if rand::random() { 0x3F } else { 0x30 };
        self.screen
            .set_pixel(self.col - 1, self.row, self.colours[noise]);
        self.col += 1;
        if self.col > HORIZONTAL_LIMIT {
            self.col = -1;
            self.row += 1;
            if self.row > VERTICAL_LIMIT {
                self.row = -1;
                self.frame_end = true;
            }
        }
    }

    pub fn frame(&mut self) {
        while self.frame_end == false {
            self.clock();
        }
        self.frame_end = false;
    }

    pub fn reset(&mut self) {}

    pub fn screen(&self) -> &Sprite {
        &self.screen
    }
}
