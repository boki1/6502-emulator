use std::cell::RefCell;
use std::rc::Rc;

use crate::nes::nes::{Nes, NesComponent};

#[derive(Debug)]
pub struct Apu {
    pub container: Option<Rc<RefCell<Nes>>>,
}

impl NesComponent for Apu {
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


impl Apu {
    pub const fn new() -> Self {
        Apu { container: None }
    }

    pub fn reset(&mut self) {}
}
