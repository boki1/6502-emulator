use std::fs::File;
use std::io::{stdout, Write};
use std::path::Path;

pub trait Bus {
    fn in_range(&self, addr: &u16) -> bool;
    fn read(&self, addr: &u16) -> u8;
    fn write(&mut self, addr: &u16, val: &u8);
    fn load_prog(&mut self, addr: &u16, prog: &[u8]);
    fn dump(&self, where_to: Option<String>);
}

#[derive(Copy, Clone, Debug)]
pub struct DummyMainBus {
    base: u16,
    limit: u16,
    space: [u8; 0xffff],
}

#[allow(dead_code)]
impl DummyMainBus {
    pub fn new() -> Self {
        DummyMainBus {
            base: 0,
            limit: 0xffff,
            space: [0 as u8; 0xffff],
        }
    }
}

impl Bus for DummyMainBus {
    fn in_range(&self, addr: &u16) -> bool {
        return *addr > self.base && *addr < self.base + self.limit;
    }

    fn read(&self, addr: &u16) -> u8 {
        let mut read: u8 = 0;
        if self.in_range(addr) {
            let idx = *addr as usize;
            read = self.space[idx]
        }
        return read;
    }

    fn write(&mut self, addr: &u16, val: &u8) {
        if self.in_range(addr) {
            let idx = *addr as usize;
            self.space[idx] = *val;
        }
    }

    fn load_prog(&mut self, addr: &u16, prog: &[u8]) {
        let mut addr: u16 = *addr;
        for byte in prog.iter() {
            self.write(&addr, &byte);
            addr += 1;
        }
    }

    fn dump(&self, where_to: Option<String>) {
        let mut out_writer = match where_to {
            Some(x) => {
                let path = Path::new(&x);
                Box::new(File::create(&path).unwrap()) as Box<dyn Write>
            }
            None => Box::new(stdout()) as Box<dyn Write>,
        };

        let control = 0x20;
        for (offset, bytev) in self.space.iter().enumerate() {
            if offset % control == 0 {
                write!(out_writer, "\n{:#04x}\t", offset).unwrap();
            }
            write!(out_writer, "{:#04x} ", bytev).unwrap();
        }
    }
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
