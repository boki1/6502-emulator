#[allow(dead_code)]
use std::cell::RefCell;
use std::rc::Rc;

use olc_pixel_game_engine::{Pixel, Sprite};

use crate::cart::cart::Cartridge;
use crate::nes::nes::{Nes, NesComponent, PPU_MIRROR, PPU_RANGE_BEGIN, PPU_RANGE_END};

const HORIZONTAL_LIMIT: i32 = 340;
const VERTICAL_LIMIT: i32 = 260;

const WIDTH: i32 = 256;
const HEIGHT: i32 = 240;

const PALETTE_RANGE_BEGIN: u16 = 0x3f00;

const PPUCTRL: u16 = 0x2000;
const PPUMASK: u16 = 0x2001;
const PPUSTATUS: u16 = 0x2002;
const OAMADDR: u16 = 0x2003;
const OAMDATA: u16 = 0x2004;
const PPUSCROLL: u16 = 0x2005;
const PPUADDR: u16 = 0x2006;
const PPUDATA: u16 = 0x2007;

/// Example:
/// #[inline]
/// fn vblank(&self) -> bool {
///     (self.0 & 1 << 7)) != 0;
/// }
macro_rules! bit {
    ($n: expr, $name: ident) => {
        #[inline]
        fn $name(&self) -> bool {
            (self.0 & (1 << $n)) != 0
        }
    };
}

/// Example:
/// #[inline]
/// fn vblank(&self, value: bool) -> bool {
///     self.0 = (self.0 & !(1 << 7)) | ((value as u8) << 7);
/// }
macro_rules! bit_setter {
    ($n: expr, $name: ident) => {
        #[inline]
        fn $name(&mut self, value: bool) {
            self.0 = (self.0 & !(1 << $n)) | ((value as u8) << $n);
        }
    };
}

/// Example:
/// #[inline]
/// fn status_reg(&self) -> &PpuStatus {
///     &self.reg_set.status_reg
/// }
macro_rules! reg_getter {
    ($name: ident, $field: ident, $type: ty) => {
        #[inline]
        fn $name(&self) -> &$type {
            &self.reg_set.$field
        }
    };
}

/// Example:
/// #[inline]
/// fn status_mut(&mut self) -> &mut PpuStatus {
///     &mut self.reg_set.status_reg
/// }
macro_rules! reg_setter {
    ($name: ident, $field: ident, $type: ty) => {
        #[inline]
        fn $name(&mut self) -> &mut $type {
            &mut self.reg_set.$field
        }
    };
}

/// Utility helper function.
/// Example:
/// bitseq(0b1000_0000_0000_0000, 15, 1) -> 1
fn bitseq_get(value: u16, offset: u16, len: u16) -> u16 {
    let mut res: u16 = 0;
    let mut mask: u16 = 1 << offset;
    let mut c = 0;
    while c < len {
        res |= value & mask;
        mask <<= 1;
        c += 1;
    }

    res
}

/// Utility helper function.
/// Example:
/// bitseq_set(0b1000_0000_0000_0000, 0, 0b1111, 4) -> 0b1000_0000_0000_1111
fn bitseq_set(mut value: u16, offset: u16, subvalue: u16, bitlen: u16) -> u16 {
    let mut mask = 1 << offset;
    let mut mask_sub = 1;
    let mut c = 0;
    while c < bitlen {
        value &= !mask;
        value |= (((subvalue & mask_sub) > 0) as u16) << (offset + c);
        mask <<= 1;
        mask_sub <<= 1;
        c += 1;
    }

    value
}

/// Example:
/// #[inline]
/// fn unused(&self) -> u16 {
///    self.0 & 0b1000_0000_0000_0000
/// }
///
/// fn unused_set(&mut self, value: u16) -> u16 {
///     self.0 = (self.0 & !())
//  self.0 = (self.0 & !(1 << $n)) | ((value as u8) << $n);
/// }

macro_rules! bitseq {
    ($offset: expr, $bitlen: expr, $name: ident) => {
        #[inline]
        fn $name(&self) -> u16 {
            bitseq_get(self.0, $offset, $bitlen)
        }
    };
}

macro_rules! bitseq_setter {
    ($offset: expr, $bitlen: expr, $name: ident) => {
        #[inline]
        fn $name(&mut self, value: u16) -> u16 {
            bitseq_set(self.0, $offset, value, $bitlen)
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

    fn set(&mut self, value: u8) {
        self.0 = value;
    }

    // Getters for the bit flags
    bit!(0, nametbl_x);
    bit!(1, nametbl_y);
    bit!(2, vertical_inc_mode);
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

    fn set(&mut self, value: u8) {
        self.0 = value;
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

    fn set(&mut self, value: u8) {
        self.0 = value;
    }

    fn observe(&mut self) -> u8 {
        let data: u8 = self.0 & 0b1110_0000;
        self.set_vblank(false);

        data
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

/// This is the structure of the so-called loopy register.
/// Check the [nesdev wiki](https://wiki.nesdev.com/w/index.php/PPU_scrolling#PPU_internal_registers) for  more details.
/// ```
/// yyy NN YYYYY XXXXX
/// ||| || ||||| +++++-- coarse X scroll
/// ||| || +++++-------- coarse Y scroll
/// ||| ++-------------- nametable select
/// +++----------------- fine Y scroll
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LoopyReg(u16);

impl LoopyReg {
    fn new() -> Self {
        Self(0)
    }

    bitseq!(0, 5, coarse_x);
    bitseq!(5, 5, coarse_y);
    bitseq!(10, 1, nametbl_x);
    bitseq!(11, 1, nametbl_y);
    bitseq!(12, 3, fine_y);

    bitseq_setter!(0, 5, coarse_x_set);
    bitseq_setter!(5, 5, coarse_y_set);
    bitseq_setter!(10, 1, nametbl_x_set);
    bitseq_setter!(11, 1, nametbl_y_set);
    bitseq_setter!(12, 3, fine_y_set);

    fn set(&mut self, value: u16) {
        self.0 = value;
    }
}

pub struct PpuRegSet {
    control_reg: PpuCtrl,
    mask_reg: PpuMask,
    status_reg: PpuStatus,
    dot: PpuDot,
    t_addr: LoopyReg,
    v_addr: LoopyReg,
}

impl PpuRegSet {
    pub fn new() -> Self {
        Self {
            dot: PpuDot::new(),
            status_reg: PpuStatus::new(),
            mask_reg: PpuMask::new(),
            control_reg: PpuCtrl::new(),
            t_addr: LoopyReg::new(),
            v_addr: LoopyReg::new(),
        }
    }

    pub fn reset(&mut self) {
        self.control_reg.set(0);
        self.mask_reg.set(0);
        self.status_reg.set(0);
        self.dot.reset();
    }
}

enum AddrLatch {
    FirstWrite,
    SecondWrite,
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

    fine_x: u8,
    data_buffer: u8,
    addr_latch: AddrLatch,
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
            data_buffer: 0,
            addr_latch: AddrLatch::FirstWrite,
            fine_x: 0,
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

    reg_getter!(status, status_reg, PpuStatus);
    reg_setter!(status_mut, status_reg, PpuStatus);

    reg_getter!(maks, mask_reg, PpuMask);
    reg_setter!(mask_mut, mask_reg, PpuMask);

    reg_getter!(control, control_reg, PpuCtrl);
    reg_setter!(control_mut, control_reg, PpuCtrl);

    reg_getter!(v_addr, v_addr, LoopyReg);
    reg_setter!(v_addr_mut, v_addr, LoopyReg);

    reg_getter!(t_addr, t_addr, LoopyReg);
    reg_setter!(t_addr_mut, t_addr, LoopyReg);

    fn scroll_t_loopy_reg(&mut self, value: u8) -> AddrLatch {
        let value_16 = value as u16;
        match &self.addr_latch {
            AddrLatch::FirstWrite => {
                self.fine_x = value & 0b0111;
                self.t_addr_mut().coarse_x_set(value_16 >> 3);
                return AddrLatch::SecondWrite;
            }
            AddrLatch::SecondWrite => {
                self.t_addr_mut().fine_y_set(value_16 & 0b0111);
                self.t_addr_mut().coarse_y_set(value_16 >> 3);
                return AddrLatch::FirstWrite;
            }
        }

        unreachable!();
    }

    fn write_addr_t_loopy_reg(&mut self, value: u8) -> AddrLatch {
        let value_16 = value as u16;
        let temp: u16 = self.t_addr().0;
        match &self.addr_latch {
            AddrLatch::FirstWrite => {
                self.t_addr_mut()
                    .set((value_16 & 0b00111111 << 8) | (temp & 0x00ff));
                return AddrLatch::SecondWrite;
            }
            AddrLatch::SecondWrite => {
                self.t_addr_mut().set((temp & 0xff00) | value_16);
                let t = self.t_addr().0;
                self.v_addr_mut().set(t);
                return AddrLatch::FirstWrite;
            }
        }

        unreachable!();
    }

    fn write_data_v_loopy_reg(&mut self, value: u8) {
        let v = self.v_addr().0;
        self.write(v, value);
        let big_increment: bool = self.control().vertical_inc_mode();
        self.v_addr_mut()
            .set(v + if big_increment { 32 } else { 1 });
    }

    fn read_data_v_loopy_reg(&mut self, addr: u16) -> u8 {
        let mut data: u8 = 0;
        data = self.data_buffer;
        self.data_buffer = self.read(self.v_addr().0);

        if addr >= PALETTE_RANGE_BEGIN {
            data = self.data_buffer;
        }

        let big_increment: bool = self.control().vertical_inc_mode();
        let v_addr_new: u16 = self.v_addr().0 + if big_increment { 32 } else { 1 };
        self.v_addr_mut().set(v_addr_new);

        data
    }

    fn update_nametbl_t_loopy_reg(&mut self, value: u8) {
        self.control_mut().set(value);
        let nt_x = self.control().nametbl_x() as u16;
        let nt_y = self.control().nametbl_y() as u16;
        self.t_addr_mut().nametbl_x_set(nt_x);
        self.t_addr_mut().nametbl_y_set(nt_y);
    }

    /// Read from PPU/secondary bus
    pub fn read(&self, addr: u16) -> u8 {
        let _valid_addr = addr & 0x3fff;
        let data = 0;

        data
    }

    /// Write to main bus
    pub fn poke_main(&mut self, addr: u16, value: u8) {
        match addr {
            PPUCTRL => self.update_nametbl_t_loopy_reg(value),
            PPUMASK => self.mask_mut().set(value),
            PPUSTATUS => {}
            OAMADDR => {}
            OAMDATA => {}
            PPUSCROLL => self.addr_latch = self.scroll_t_loopy_reg(value),
            PPUADDR => self.addr_latch = self.write_addr_t_loopy_reg(value),
            PPUDATA => self.write_data_v_loopy_reg(value),
            _ => unreachable!(),
        }
    }

    /// Read from main bus
    pub fn peek_main(&mut self, addr: u16) -> u8 {
        let mut data: u8 = 0;

        match addr {
            PPUCTRL => { /* unreadable */ }
            PPUMASK => { /* unreadable */ }
            PPUSTATUS => {
                data = self.status_mut().observe();
                self.addr_latch = AddrLatch::FirstWrite;
            }
            OAMADDR => { /* unreadable */ }
            OAMDATA => { /* unreadable */ }
            PPUSCROLL => { /* unreadable */ }
            PPUADDR => { /* unreadable */ }
            PPUDATA => data = self.read_data_v_loopy_reg(addr),
            _ => unreachable!(),
        }

        data
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
