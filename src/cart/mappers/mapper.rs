pub trait Mapper {
    fn map_read_main(&self, addr: u16) -> Option<u16>;
    fn map_write_main(&self, addr: u16) -> Option<u16>;
    fn map_read_sec(&self, addr: u16) -> Option<u16>;
    fn map_write_sec(&self, addr: u16) -> Option<u16>;
}
