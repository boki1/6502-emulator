extern crate m6502;
extern crate olc_pixel_game_engine;

use crate::olc_pixel_game_engine as olc;
use m6502::mos6502::{Address, Asm, Cpu, CpuError};

enum WindowView {
    CpuMonitor,
    MemoryMonitor(Address),
}

struct GCDExample {
    cpu: Cpu,
    source_file: String,
    disassembly: Asm,
    pov: WindowView,
    finished: bool,
    started: bool,
    continuing: bool,
    initial_pc: u16,
}

impl GCDExample {
    fn new(source_file: &str) -> Self {
        Self {
            cpu: Cpu::default(),
            source_file: source_file.to_string(),
            disassembly: Asm::default(),
            pov: WindowView::CpuMonitor,
            finished: false,
            started: false,
            continuing: false,
            initial_pc: 0x00,
        }
    }
}

impl olc::Application for GCDExample {
    fn on_user_create(&mut self) -> Result<(), olc::Error> {
        if let Err(CpuError::FailedLoadingProgram) =
            self.cpu.load_file(&self.source_file, self.initial_pc, true)
        {
            panic!("Failed opening source file.");
        }

        self.disassembly = Asm::from_addr_range(&mut self.cpu, self.initial_pc, 45);
        let regs = self.cpu.regset_mut();
        regs.set_prog_counter(0x00);
        regs.set_accumulator(0xFF);
        regs.set_x_index(0xFF);
        regs.set_y_index(0xFF);

        Ok(())
    }

    fn on_user_update(&mut self, _elapsed_time: f32) -> Result<(), olc::Error> {
        olc::clear(olc::BLACK);

        match self.pov {
            WindowView::CpuMonitor => self.draw_cpu_monitor()?,
            WindowView::MemoryMonitor(beginning) => self.draw_mem(beginning)?,
        };

        self.finished = self.cpu.pc() == 0x28;

        // Next instruction
        if !self.finished {
            if self.continuing || olc::get_key(olc::Key::SPACE).released {
                self.cpu.full_instruction();
                self.started = true;
            }

            // Continue execution
            if olc::get_key(olc::Key::E).released {
                self.continuing = true;
            }
        }

        // Reset cpu
        if olc::get_key(olc::Key::R).released {
            self.cpu.reset();
            self.continuing = false;
            self.started = false;
            self.cpu.regset_mut().set_prog_counter(self.initial_pc);
        }

        // Watch cpu
        if olc::get_key(olc::Key::C).released {
            self.pov = WindowView::CpuMonitor;
        }

        // Watch memory
        if olc::get_key(olc::Key::M).released {
            self.pov = WindowView::MemoryMonitor(0x0000);
        }

        Ok(())
    }

    fn on_user_destroy(&mut self) -> Result<(), olc::Error> {
        Ok(())
    }
}

impl GCDExample {
    fn draw_cpu_monitor(&mut self) -> Result<(), olc::Error> {
        let pc = self.cpu.pc();

        // Draw all instructions
        let mut this_y = 0;
        let mut col: olc::Pixel;
        for (idx, instr) in self.disassembly.code().iter().enumerate() {
            let y = 10 + (idx as i32 * 10);
            if instr.load_address() == pc {
                this_y = y + 2;
                col = olc::GREY;
            } else {
                col = olc::WHITE;
            }
            olc::draw_string(20, y, &format!("{}", instr), col)?;
        }

        // Draw marking of current instruction
        if this_y > 0 {
            olc::fill_circle(3, this_y, 2, olc::GREEN);
        }

        let separator_y = 350;

        // Separator
        olc::draw_line(0, separator_y, 260, separator_y, olc::WHITE);

        // Monitor
        let regs = self.cpu.regset();
        olc::draw_string(
            20,
            separator_y + 10,
            &format!(
                "A = {:#4X?}    SP = {:#4X?}",
                regs.accumulator(),
                regs.stk_ptr()
            ),
            olc::WHITE,
        )?;
        olc::draw_string(
            20,
            separator_y + 20,
            &format!(
                "X = {:#04X?}    PC = {:#06X?}",
                regs.x_index(),
                regs.prog_counter()
            ),
            olc::WHITE,
        )?;
        olc::draw_string(
            20,
            separator_y + 30,
            &format!(
                "Y = {:#04X?}    PS = {:#04X?}",
                regs.y_index(),
                regs.status()
            ),
            olc::WHITE,
        )?;

        // Separator
        let col = if self.finished {
            olc::GREEN
        } else {
            olc::DARK_CYAN
        };
        olc::draw_line(0, separator_y + 40, 260, separator_y + 40, col);
        olc::draw_string(66, separator_y + 50, &format!("CPU Monitor"), col)?;

        Ok(())
    }

    fn draw_mem(&mut self, begin_address: Address) -> Result<(), olc::Error> {
        let mut x = 60;
        let mut y = 10;

        olc::draw_string(0, 10, &format!("{:#06X?}:   ", begin_address), olc::YELLOW)?;
        let mut address = begin_address;
        let end_address = begin_address + 160;
        while address < end_address {
            if address % 8 == 0 && address != begin_address {
                y += 10;
                x = 60;
                olc::draw_string(0, y, &format!("{:#06X?}:   ", address), olc::YELLOW)?;
            }

            let data = self.cpu.read_word(address);
            olc::draw_string(x, y, &format!("{:#06x?}", data.to_be()), olc::WHITE)?;
            x += 50;

            address += 2;
        }

        // Separator
        let col = if self.finished {
            olc::GREEN
        } else {
            olc::YELLOW
        };
        olc::draw_line(0, 240, 260, 240, col);
        olc::draw_string(66, 245, &format!("Mem Monitor"), col)?;

        Ok(())
    }
}

fn main() {
    let mut fib_program = GCDExample::new("src/gcd.bin");

    olc::start("Sieve example", &mut fib_program, 400, 500, 2, 2).unwrap();
}
