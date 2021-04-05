use std::cell::RefCell;
use std::rc::Rc;

use crate::apu::apu_2a03::Apu;
use crate::cart::cart::Cartridge;
use crate::cpu::mos6502::Cpu;
use crate::ppu::ppu_2c02::Ppu;

pub const RAM_BEGIN: u16 = 0x00;
pub const RAM_END: u16 = 0x1fff;
pub const RAM_MIRROR: u16 = 0x07ff;

pub const PPU_RANGE_BEGIN: u16 = 0x2000;
pub const PPU_RANGE_END: u16 = 0x3fff;
pub const PPU_MIRROR: u16 = 0x7;

pub const CONTROLLER_RANGE_BEGIN: u16 = 0x4016;
pub const CONTROLLER_RANGE_END: u16 = 0x4017;

// About reading: whether we observe the value or just read it for debugging purposes.
pub const HAS_SIDE_EFFECT: bool = false;
pub const HAS_NO_SIDE_EFFECT: bool = true;

pub struct Nes {
    pub cpu: Cpu,
    pub ppu: Ppu,
    pub apu: Apu,
    pub ram: [u8; 2048],
    pub cartridge: Option<Rc<RefCell<Cartridge>>>,
    sys_clocks: usize,
}

impl std::fmt::Debug for Nes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Nes")
            .field("cpu", &self.cpu)
            .field("ppu", &self.ppu)
            .field("apu", &self.apu)
            .field("sys_clocks", &self.sys_clocks)
            .finish()
    }
}

impl Nes {
    pub fn new_wrapper() -> Rc<RefCell<Nes>> {
        let container = Rc::new(RefCell::new(Nes {
            cpu: Cpu::new(),
            ppu: Ppu::new(None),
            apu: Apu::new(),
            ram: [0; 2048],
            cartridge: None,
            sys_clocks: 0,
        }));

        {
            let cpu = &mut container.borrow_mut().cpu;
            cpu.attach_to(container.clone());
        }

        {
            let ppu = &mut container.borrow_mut().ppu;
            ppu.attach_to(container.clone());
        }

        {
            let apu = &mut container.borrow_mut().apu;
            apu.attach_to(container.clone());
        }

        container.clone()
    }

    pub fn insert_cart(&mut self, cart: Rc<RefCell<Cartridge>>) {
        self.cartridge = Some(cart);
    }

    pub fn reset(&mut self) {
        let cpu_ = self.cpu_mut();
        cpu_.reset();

        let ppu_ = self.ppu_mut();
        ppu_.reset();

        let apu_ = self.apu_mut();
        apu_.reset();

        self.sys_clocks = 0;
    }

    pub fn sys_clock(&mut self) {
        self.ppu_mut().clock();

        if self.sys_clocks % 3 == 0 {
            self.cpu_mut().tick();
        }

        self.sys_clocks += 1;
    }

    pub fn cpu(&self) -> &'_ Cpu {
        &self.cpu
    }
    pub fn cpu_mut(&mut self) -> &'_ mut Cpu {
        &mut self.cpu
    }

    pub fn ppu(&self) -> &'_ Ppu {
        &self.ppu
    }
    pub fn ppu_mut(&mut self) -> &'_ mut Ppu {
        &mut self.ppu
    }

    pub fn apu(&self) -> &'_ Apu {
        &self.apu
    }

    pub fn apu_mut(&mut self) -> &'_ mut Apu {
        &mut self.apu
    }
}

impl Nes {
    /// Initiate read from the main nes bus (or cartridge).
    /// When the CPU and the PPU communicate, the PPU's registers update their values. Hence this
    /// method needs a mutable reference.
    pub fn read(&mut self, addr: u16, no_side_effect: bool) -> u8 {
        let mut read: u8 = 0;
        let mut cart_handle: bool = false;

        if let Some(cart) = &self.cartridge {
            // let addr = addr % 0x4000;
            if let Some(read_from_cart) = cart.borrow_mut().prg_mem_read(addr) {
                cart_handle = true;
                read = read_from_cart;
            }
        }

        let mut mirrored_addr = addr;

        if !cart_handle {
            // The first check is implied by the type limit.
            // ''' addr >= RAM_BEGIN &&  '''
            if (RAM_BEGIN..=RAM_END).contains(&addr) {
                mirrored_addr %= RAM_MIRROR;
                read = self.ram[mirrored_addr as usize];
            } else if (PPU_RANGE_BEGIN..=PPU_RANGE_END).contains(&addr) {
                mirrored_addr %= PPU_MIRROR;
                read = self.ppu_mut().peek_main(mirrored_addr, no_side_effect);
            } else if (CONTROLLER_RANGE_BEGIN..=CONTROLLER_RANGE_END).contains(&addr) {
                // TODO:
                // Implement controller sensitivity
                // todo!();
            }
        }

        read
    }

    /// Initiate write to the main nes bus (or cartridge).
    pub fn write(&mut self, addr: u16, data: u8) {
        let mut cart_handle: bool = false;
        if let Some(cart) = &mut self.cartridge {
            // let addr = addr % 0x4000;
            cart.borrow_mut().prg_mem_writ(addr, data);
            cart_handle = true;
        }

        if cart_handle == false {
            // The first check is implied by the type limit.
            // ''' addr >= RAM_BEGIN &&  '''

            if (RAM_BEGIN..=RAM_END).contains(&addr) {
                self.ram[(addr & RAM_MIRROR) as usize] = data;
            } else if (PPU_RANGE_BEGIN..=PPU_RANGE_END).contains(&addr) {
                self.ppu_mut().poke_main(addr & PPU_MIRROR, data);
            } else if (CONTROLLER_RANGE_BEGIN..=CONTROLLER_RANGE_END).contains(&addr) {
                // TODO:
                // Implement controller sensitivity
                todo!();
            }
        }
    }
}

/// A trait implemented by all NES components.
pub trait NesComponent {
    type NesConsole;
    type NesConsolePtr;

    fn attach_to(&mut self, container: Self::NesConsolePtr);
    fn container_of(&self) -> &Self::NesConsole;
    fn container_of_mut(&mut self) -> &mut Self::NesConsole;
}
