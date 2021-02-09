use std::fs::File;
use std::io::Read;

use ines::{Ines, Mapper, Mirroring};

use super::mappers::mapper::MapperAccess;
use super::mappers::mapper_nrom::*;

pub struct Cartridge {
    mapper: Box<dyn MapperAccess>,
    ines: Ines,
}

impl Cartridge {
    pub fn from_ines(ines: Ines) -> Self {
        let mapper: Box<dyn MapperAccess>;
        match ines.header.mapper {
            Mapper::Nrom => {
                mapper = Box::new(Nrom::new(
                    ines.header.num_prg_rom_blocks as u16,
                    ines.header.num_chr_rom_blocks as u16,
                ))
            }
            _ => panic!("Unsupported mapper"),
        };

        println!(
            "PRG_ROM={}B\nCHR_ROM={}B",
            ines.prg_rom.len(),
            ines.chr_rom.len()
        );

        Self { mapper, ines }
    }

    pub fn from_filename(fname: &'_ str) -> Self {
        let mut file = File::open(fname).expect("Failed opening gamepack");
        let mut buffer = Vec::new();

        file.read_to_end(&mut buffer)
            .expect("Failed reading gamepack");

        Self::from_ines(Ines::parse(&buffer).expect("Failed processing gamepack"))
    }

    #[inline]
    fn prg_mem(&self) -> &Vec<u8> {
        &self.ines.prg_rom
    }

    #[inline]
    fn prg_mem_mut(&mut self) -> &mut Vec<u8> {
        &mut self.ines.prg_rom
    }

    #[inline]
    fn chr_mem(&self) -> &Vec<u8> {
        &self.ines.chr_rom
    }

    #[inline]
    fn chr_mem_mut(&mut self) -> &mut Vec<u8> {
        &mut self.ines.chr_rom
    }

    #[inline]
    pub fn mirroring(&self) -> Mirroring {
        self.ines.header.mirroring
    }

    #[inline]
    pub fn mapper(&self) -> &dyn MapperAccess {
        &(*self.mapper)
    }

    pub fn prg_mem_read(&self, addr: u16) -> Option<u8> {
        if let Some(mapped_addr) = self.mapper().map_read_main(addr) {
            return Some(self.prg_mem()[mapped_addr as usize]);
        }
        None
    }

    pub fn prg_mem_writ(&mut self, addr: u16, data: u8) {
        if let Some(mapped_addr) = self.mapper().map_read_main(addr) {
            self.prg_mem_mut()[mapped_addr as usize] = data;
        }
    }

    pub fn chr_mem_read(&self, addr: u16) -> Option<u8> {
        if let Some(mapped_addr) = self.mapper().map_read_sec(addr) {
            return Some(self.chr_mem()[mapped_addr as usize]);
        }
        None
    }

    pub fn chr_mem_writ(&mut self, addr: u16, data: u8) {
        if let Some(mapped_addr) = self.mapper().map_read_sec(addr) {
            self.chr_mem_mut()[mapped_addr as usize] = data;
        }
    }
}
