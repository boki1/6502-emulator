pub trait Bus {
    fn read(&self, addr: &u16) -> u8;
    fn write(&mut self, addr: &u16, val: &u8);
}

#[derive(Copy, Clone, Debug)]
pub struct DummyMainBus {
    space: [u8; 0xffff],
}

#[allow(dead_code)]
impl DummyMainBus {
    pub fn new_test() -> Self {
        let mut a = DummyMainBus {
            space: [0 as u8; 0xffff],
        };

        // Exir
        a.space[0x24] = 0x74;
        a.space[0x25] = 0x20;

        // Irex
        a.space[0x86] = 0x28;
        a.space[0x87] = 0x40;

        a
    }

    pub fn new() -> Self {
        DummyMainBus {
            space: [0 as u8; 0xffff],
        }
    }
}

impl Bus for DummyMainBus {
    fn read(&self, addr: &u16) -> u8 {
        let idx = *addr as usize;
        // NB: u16 is always in the interval of u16 values
        self.space[idx]
    }

    fn write(&mut self, addr: &u16, val: &u8) {
        // NB: u16 is always in the interval of u16 values
        let idx = *addr as usize;
        self.space[idx] = *val;
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
