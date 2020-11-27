pub trait Bus {
    fn read(&self, addr: &u16) -> u8;
    fn write(&mut self, addr: &u16, val: &u8);
    fn load_prog(&mut self, addr: &u16, prog: &[u8]);
    fn dump(&self);
}

#[derive(Copy, Clone, Debug)]
pub struct DummyMainBus {
    space: [u8; 0x07ff],
}

#[allow(dead_code)]
impl DummyMainBus {
    pub fn new() -> Self {
        DummyMainBus {
            space: [0 as u8; 0x07ff],
        }
    }
}

impl Bus for DummyMainBus {
    fn read(&self, addr: &u16) -> u8 {
        let idx = *addr as usize;
        if idx > 0x07ff {
            return 1;
        }
        // NB: u16 is always in the interval of u16 values
        self.space[idx]
    }

    fn write(&mut self, addr: &u16, val: &u8) {
        // NB: u16 is always in the interval of u16 values
        let idx = *addr as usize;
        if idx > 0x07ff {
            return;
        }
        self.space[idx] = *val;
    }

    fn load_prog(&mut self, addr: &u16, prog: &[u8]) {
        let mut addr: u16 = *addr;
        for byte in prog.iter() {
            self.write(&addr, &byte);
            addr += 1;
        }
    }

    fn dump(&self) {
        for (offset, bytev) in self.space.iter().enumerate() {
            if offset % 0x20 == 0 {
                print!("\n{:#04x}\t", offset);
            }
            print!("{:#04x} ", bytev);
        }
    }
}

#[test]
fn test_init_bus() {
    let b = DummyMainBus::new();
    assert_eq!(vec!(b.space), vec!([0; 0xffff]));
}

#[test]
fn test_read() {
    let b = DummyMainBus::new();
    let addr = 0x4151;
    let read = b.read(&addr);
    assert_eq!(read, 0);
}

#[test]
fn test_write_and_read() {
    let mut b = DummyMainBus::new();
    let addr = 0x4151;
    let val = 7;
    b.write(&addr, &val);
    let read = b.read(&addr);
    assert_eq!(read, val);
}
