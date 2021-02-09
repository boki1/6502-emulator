use super::mapper::MapperAccess;

pub struct Nrom {
    pub prog_banks: u16,
    pub char_banks: u16,
}

impl Nrom {
    pub fn new(prog_banks: u16, char_banks: u16) -> Self {
        Self {
            prog_banks,
            char_banks,
        }
    }
}

impl MapperAccess for Nrom {
    // if PRGROM is 16KB
    //     CPU Address Bus          PRG ROM
    //     0x8000 -> 0xBFFF: Map    0x0000 -> 0x3FFF
    //     0xC000 -> 0xFFFF: Mirror 0x0000 -> 0x3FFF
    // if PRGROM is 32KB
    //     CPU Address Bus          PRG ROM
    //     0x8000 -> 0xFFFF: Map    0x0000 -> 0x7FFF

    fn map_read_main(&self, addr: u16) -> Option<u16> {
        if addr >= 0x8000 {
            let mask: u16 = if self.prog_banks > 1 { 0x7fff } else { 0x3fff };
            let mapped = addr & mask;
            return Some(mapped);
        }
        None
    }
    fn map_write_main(&self, addr: u16) -> Option<u16> {
        if addr >= 0x8000 {
            let mask: u16 = if self.prog_banks > 1 { 0x7fff } else { 0x3fff };
            return Some(addr & mask);
        }
        None
    }

    // There is no mapping required for PPU
    // PPU Address Bus          CHR ROM
    // 0x0000 -> 0x1FFF: Map    0x0000 -> 0x1FFF
    fn map_read_sec(&self, addr: u16) -> Option<u16> {
        if addr <= 0x1fff {
            return Some(addr);
        }
        None
    }

    fn map_write_sec(&self, addr: u16) -> Option<u16> {
        if addr <= 0x1fff {
            if self.char_banks == 0 {
                // As RAM
                return Some(addr);
            }
        }
        None
    }
}
