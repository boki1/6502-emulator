#[allow(dead_code)]
use std::cell::RefCell;
use std::rc::Rc;

use olc_pixel_game_engine::{Pixel, Sprite};

use crate::cart::cart::Cartridge;
use crate::nes::nes::{Nes, NesComponent};

const HORIZONTAL_LIMIT: i32 = 340;
const VERTICAL_LIMIT: i32 = 260;

pub const WIDTH: i32 = 256;
pub const HEIGHT: i32 = 240;

macro_rules! bit {
    ($n: expr, $name: ident) => {
        #[inline]
        fn $name(&self) -> bool {
            (self.0 & (1 << $n)) != 0
        }
    };
}

macro_rules! bit_setter {
    ($n: expr, $name: ident) => {
        #[inline]
        fn $name(&mut self, value: bool) {
            self.0 = (self.0 & !(1 << $n)) | ((value as u8) << $n);
        }
    };
}

/// Registers
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct PpuCtrl(u8);

impl PpuCtrl {
    fn new() -> Self {
        Self(0)
    }

    fn reset(&mut self) {
        self.0 = 0;
    }

    // Getters for the bit flags
    bit!(0, nametbl_x);
    bit!(1, nametbl_y);
    bit!(2, vram_increment_mode);
    bit!(3, pattern_tbl_fg);
    bit!(4, pattern_tbl_bg);
    bit!(5, big_foreground);
    bit!(6, slave_mode); // Unused
    bit!(7, nmi_enabled);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct PpuMask(u8);

impl PpuMask {
    fn new() -> Self {
        Self(0)
    }

    fn reset(&mut self) {
        self.0 = 0;
    }

    bit!(0, grayscale_enabled);
    bit!(1, render_bg_left);
    bit!(2, render_fg_left);
    bit!(3, render_bg);
    bit!(4, render_fg);
    bit!(5, enhance_red);
    bit!(6, enhance_green);
    bit!(7, enhance_blue);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct PpuStatus(u8);

impl PpuStatus {
    fn new() -> Self {
        Self(0)
    }

    fn reset(&mut self) {
        self.0 = 0;
    }

    // First 5 bits are unused
    bit!(5, fg_overflow);

    bit!(6, fg_zero_hit);
    bit_setter!(6, set_fg_zero_hit);

    bit!(7, vblank);
    bit_setter!(7, set_vblank);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct PpuDot {
    scanline: i32,
    cycles: i32,
}

impl PpuDot {
    fn new() -> Self {
        Self {
            scanline: 261,
            cycles: 0,
        }
    }

    #[inline]
    fn cycles(&self) -> i32 {
        self.cycles
    }

    #[inline]
    fn scanline(&self) -> i32 {
        self.scanline
    }

    /// Updates the renderer and notifies whether the frame has ended
    fn update(&mut self) -> bool {
        self.cycles += 1;
        if self.cycles > HORIZONTAL_LIMIT {
            self.cycles = 0;
            self.scanline += 1;
            if self.scanline > VERTICAL_LIMIT {
                self.scanline = -1;
                return true;
            }
        }
        false
    }

    #[inline]
    fn reset_cycles(&mut self) {
        self.cycles = 0;
    }

    #[inline]
    fn reset_scanline(&mut self) {
        self.scanline = -1;
    }

    #[inline]
    fn reset(&mut self) {
        self.reset_cycles();
        self.reset_scanline();
    }
}

pub struct PpuRegSet {
    control_reg: PpuCtrl,
    mask_reg: PpuMask,
    status_reg: PpuStatus,
    dot: PpuDot,
}

impl PpuRegSet {
    pub fn new() -> Self {
        Self {
            dot: PpuDot::new(),
            status_reg: PpuStatus::new(),
            mask_reg: PpuMask::new(),
            control_reg: PpuCtrl::new(),
        }
    }

    pub fn reset(&mut self) {
        self.control_reg.reset();
        self.mask_reg.reset();
        self.status_reg.reset();
        self.dot.reset();
    }
}

/// The Picture processing unit
pub struct Ppu {
    /// Connections
    container: Option<Rc<RefCell<Nes>>>,
    cart: Option<Rc<Cartridge>>,

    /// Ppu bus
    pattern_mem: [u8; 8 * 1024],
    vram: [u8; 2 * 1024],
    palette_mem: [u8; 32],
    colours: [Pixel; 64],

    /// Internal states
    screen: Sprite,
    reg_set: PpuRegSet,
    frame_end: bool,
}

impl std::fmt::Debug for Ppu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Ppu")
            .field("scanline", &self.scanline())
            .field("cycle", &self.cycles())
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
            frame_end: false,
            reg_set: PpuRegSet::new(),
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

    #[inline]
    pub fn cycles(&self) -> i32 {
        self.reg_set.dot.cycles()
    }

    #[inline]
    pub fn scanline(&self) -> i32 {
        self.reg_set.dot.scanline()
    }

    #[inline]
    fn dot_mut(&mut self) -> &'_ mut PpuDot {
        &mut self.reg_set.dot
    }

    /// Write to PPU/secondary bus
    pub fn write(&mut self, addr: u16, _val: u8) {
        let _valid_addr = addr & 0x3fff;
    }

    #[inline]
    pub fn frame_has_ended(&self) -> bool {
        self.frame_end
    }

    #[inline]
    pub fn frame_reset(&mut self) {
        self.frame_end = false;
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
        // Generate random noise
        // let noise = if rand::random() { 0x3F } else { 0x30 };
        // self.screen.set_pixel(
        //     self.reg_set.dot.cycles() - 1,
        //     self.reg_set.dot.scanline(),
        // self.colours[noise],
        // );
        // ----

        self.frame_end = self.dot_mut().update();
    }

    pub fn full_frame(&mut self) {
        while self.frame_has_ended() == false {
            self.clock();
        }
        self.frame_reset();
    }

    pub fn reset(&mut self) {
        self.reg_set.reset();
    }

    pub fn screen(&self) -> &Sprite {
        &self.screen
    }
}
