extern crate m6502;
extern crate olc_pixel_game_engine;

use crate::olc_pixel_game_engine as olc;
use m6502::mos6502::{Address, Asm, Cpu, CpuError};

enum FibonaciiExampleView {
    CpuMonitor,
    MemoryMonitor(Address),
}

struct FibonacciExample {
    cpu: Cpu,
    source_file: String,
    disassembly: Asm,
    pov: FibonaciiExampleView,
    finished: bool,
    continuing: bool,
}

impl FibonacciExample {
    fn new(source_file: &str) -> Self {
        Self {
            cpu: Cpu::default(),
            source_file: source_file.to_string(),
            disassembly: Asm::default(),
            pov: FibonaciiExampleView::CpuMonitor,
            finished: false,
            continuing: false,
        }
    }
}

impl olc::Application for FibonacciExample {
    fn on_user_create(&mut self) -> Result<(), olc::Error> {
        if let Err(CpuError::FailedLoadingProgram) =
            self.cpu.load_file(&self.source_file, 0x8000, true)
        {
            panic!("Failed opening source file.");
        }

        self.disassembly = Asm::from_addr_range(&mut self.cpu, 0x8000, 32);
        let regs = self.cpu.regset_mut();
        regs.set_prog_counter(0x8000);
        regs.set_accumulator(0xFF);
        regs.set_x_index(0xEE);
        regs.set_y_index(0xDD);

        Ok(())
    }

    fn on_user_update(&mut self, _elapsed_time: f32) -> Result<(), olc::Error> {
        olc::clear(olc::BLACK);

        match self.pov {
            FibonaciiExampleView::CpuMonitor => self.draw_cpu_monitor(),
            FibonaciiExampleView::MemoryMonitor(beginning) => self.draw_mem(beginning),
        };

        self.finished = (self.cpu.pc() == 0x0000);

        // Next instruction
        if !self.finished {
            if self.continuing || olc::get_key(olc::Key::SPACE).released {
                self.cpu.full_instruction();
            }

            // Continue execution
            if olc::get_key(olc::Key::E).released {
                self.continuing = true;
            }
        }

        // Reset cpu
        if olc::get_key(olc::Key::R).released {
            self.cpu.reset();
        }

        // Watch cpu
        if olc::get_key(olc::Key::C).released {
            self.pov = FibonaciiExampleView::CpuMonitor;
        }

        // Watch memory
        if olc::get_key(olc::Key::M).released {
            self.pov = FibonaciiExampleView::MemoryMonitor(0xE0);
        }

        Ok(())
    }

    fn on_user_destroy(&mut self) -> Result<(), olc::Error> {
        Ok(())
    }
}

impl FibonacciExample {
    fn draw_cpu_monitor(&mut self) -> Result<(), olc::Error> {
        let pc = self.cpu.pc();

        // Draw all instructions
        let mut this_y = 0;
        for (idx, instr) in self.disassembly.code().iter().enumerate() {
            let y = 10 + (idx as i32 * 10);
            olc::draw_string(20, y, &format!("{}", instr), olc::WHITE)?;
            if instr.load_address() == pc {
                this_y = y + 2;
            }
        }

        // Draw marking of current instruction
        if this_y > 0 {
            olc::fill_circle(3, this_y, 2, olc::GREEN);
        }

        // Separator
        olc::draw_line(0, 190, 260, 190, olc::WHITE);

        // Monitor
        let regs = self.cpu.regset();
        olc::draw_string(
            20,
            200,
            &format!(
                "A = {:#4X?}    SP = {:#4X?}",
                regs.accumulator(),
                regs.stk_ptr()
            ),
            olc::WHITE,
        )?;
        olc::draw_string(
            20,
            210,
            &format!(
                "X = {:#04X?}    PC = {:#06X?}",
                regs.x_index(),
                regs.prog_counter()
            ),
            olc::WHITE,
        )?;
        olc::draw_string(
            20,
            220,
            &format!("Y = {:#04X?}    PS = {:#04X?}", regs.y_index(), regs.status()),
            olc::WHITE,
        )?;

        // Separator
        let col = if self.finished {
            olc::GREEN
        } else {
            olc::DARK_CYAN
        };
        olc::draw_line(0, 240, 260, 240, col);
        olc::draw_string(66, 245, &format!("CPU Monitor"), col);

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
        olc::draw_string(66, 245, &format!("Mem Monitor"), col);

        Ok(())
    }
}

fn main() {
    let mut fib_program = FibonacciExample::new("src/fib.bin");

    olc::start("Fibonacci example", &mut fib_program, 400, 260, 2, 2).unwrap();
}
