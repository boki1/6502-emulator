use std::borrow::BorrowMut;
use std::fs::File;
use std::io::Read;
use std::slice;

use super::mappers::mapper::Mapper;
use super::mappers::mapper_nrom::*;

const PROG_ROM_CHUNK_SIZE: u16 = 16 * 1024;
const CHAR_ROM_CHUNK_SIZE: u16 = 8 * 1024;

pub struct Cartridge {
    pub mapper: Box<dyn Mapper>,
    prog_mem: Vec<u8>,
    char_mem: Vec<u8>,
    prog_chunks: u16,
    char_chunks: u16,
    mapper_id: u8,
    mirror: MirrorKind,
    is_valid: bool,
}

enum MirrorKind {
    Horizontal,
    Vertical,
}

// iNES Format Header
#[repr(C)]
struct ines_header {
    name: [u8; 4],
    prog_rom_chunks: u8,
    char_rom_chunks: u8,
    mapper1: u8,
    mapper2: u8,
    prog_ram_size: u8,
    tv_system1: u8,
    tv_system2: u8,
    padding: [u8; 5],
}

const BUFFER_SIZE: usize = 256;

impl Cartridge {
    // TODO:
    // handle errors; remove `unwrap()`'s; `loop {}`
    pub fn new(fname: &str) -> Self {
        let mut fhandle = File::open(fname).unwrap();
        let mut buf = [0u8; BUFFER_SIZE];

        fn read_struct<T, R: Read>(src: &mut R) -> std::io::Result<T> {
            let limit = ::std::mem::size_of::<T>();
            unsafe {
                let mut ptr = std::mem::MaybeUninit::<T>::uninit();
                let buff = slice::from_raw_parts_mut(ptr.as_mut_ptr() as *mut u8, limit);
                // let buff = slice::from_raw_parts_mut(&mut ptr as *mut T as *mut u8, limit);
                match src.read_exact(buff) {
                    Ok(()) => Ok(ptr.assume_init()),
                    Err(e) => {
                        ::std::mem::forget(ptr);
                        Err(e)
                    }
                }
            }
        }

        let header: ines_header;
        if let Ok(h) = read_struct::<ines_header, File>(fhandle.borrow_mut()) {
            header = h;
        } else {
            loop {}
        }

        if header.mapper1 & 0x04 != 0 {
            let _ = fhandle.by_ref().take(512).read(&mut buf).unwrap();
        }

        let mapper_id: u8;
        let mapper: Box<dyn Mapper>;
        let mirror: MirrorKind;
        let is_valid: bool;
        let prog_chunks: u16;
        let char_chunks: u16;

        mapper_id = ((header.mapper1 >> 4) << 4) as u8 | (header.mapper2 >> 4) as u8;
        mirror = if header.mapper1 & 0x01 != 0 {
            MirrorKind::Vertical
        } else {
            MirrorKind::Horizontal
        };
        prog_chunks = header.prog_rom_chunks as u16;
        char_chunks = header.char_rom_chunks as u16;

        is_valid = match mapper_id {
            0 => {
                mapper = Box::new(Nrom::new(prog_chunks, char_chunks));
                true
            }
            _ => {
                mapper = Box::new(Nrom::new(prog_chunks, char_chunks));
                false
            }
        };

        let mut self_ = Self {
            mapper,
            prog_mem: vec![],
            char_mem: vec![],
            prog_chunks,
            char_chunks,
            mapper_id,
            mirror,
            is_valid,
        };

        let prog_mem_len = (self_.prog_chunks * PROG_ROM_CHUNK_SIZE) as u64;
        self_.prog_mem.resize(prog_mem_len as usize, 0);
        let prog_mem_ptr = self_.prog_mem.as_mut_slice();
        let _ = fhandle
            .by_ref()
            .take(prog_mem_len)
            .read(prog_mem_ptr)
            .unwrap();

        let char_mem_len = (self_.char_chunks * CHAR_ROM_CHUNK_SIZE) as u64;
        self_.char_mem.resize(char_mem_len as usize, 0);
        let char_mem_ptr = self_.char_mem.as_mut_slice();
        let _ = fhandle
            .by_ref()
            .take(char_mem_len)
            .read(char_mem_ptr)
            .unwrap();

        self_
    }

    pub fn is_ok(&self) -> bool {
        self.is_valid
    }

    pub fn prg_mem_read(&self, addr: u16) -> Option<u8> {
        if let Some(mapped_addr) = (*self.mapper).map_read_main(addr) {
            return Some(self.prog_mem[mapped_addr as usize]);
        }
        None
    }

    pub fn prg_mem_writ(&mut self, addr: u16, data: u8) {
        if let Some(mapped_addr) = (*self.mapper).map_read_main(addr) {
            self.prog_mem[mapped_addr as usize] = data;
        }
    }

    pub fn chr_mem_read(&self, addr: u16) -> Option<u8> {
        if let Some(mapped_addr) = (*self.mapper).map_read_sec(addr) {
            return Some(self.char_mem[mapped_addr as usize]);
        }
        None
    }

    pub fn chr_mem_writ(&mut self, addr: u16, data: u8) {
        if let Some(mapped_addr) = (*self.mapper).map_read_sec(addr) {
            self.char_mem[mapped_addr as usize] = data;
        }
    }
}
