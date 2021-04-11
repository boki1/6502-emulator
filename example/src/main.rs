extern crate m6502;
extern crate olc_pixel_game_engine;

use crate::olc_pixel_game_engine as olc;
use m6502::mos6502::{Asm, Cpu, CpuError, Address};

enum FibonaciiExampleView {
    CpuMonitor,
    MemoryMonitor(Address),
}

struct FibonacciExample {
    cpu: Cpu,
    source_file: String,
    disassembly: Asm,
    pov: FibonaciiExampleView
}

impl FibonacciExample {
    fn new(source_file: &str) -> Self {

        Self {
            cpu: Cpu::default(),
            source_file: source_file.to_string(),
            disassembly: Asm::default(),
            pov: FibonaciiExampleView::CpuMonitor,
        }
    }
}

impl olc::Application for FibonacciExample {
    fn on_user_create(&mut self) -> Result<(), olc::Error> {

        if let Err(CpuError::FailedLoadingProgram) = self.cpu.load_file(&self.source_file, 0x8000, true) {
            panic!("Failed opening source file.");
        }

        self.disassembly = Asm::from_addr_range(&mut self.cpu, 0x8000, 32);
        self.cpu.regset_mut().set_prog_counter(0x8000);

        Ok(())
    }

    fn on_user_update(&mut self, _elapsed_time: f32) -> Result<(), olc::Error> {
        olc::clear(olc::BLACK);

        match self.pov {
            FibonaciiExampleView::CpuMonitor => self.draw_cpu_monitor(),
            FibonaciiExampleView::MemoryMonitor(beginning) => self.draw_mem(beginning),
        };

        // Next instruction
        if olc::get_key(olc::Key::SPACE).released {
            self.cpu.clock_cycle();
        }

        if olc::get_key(olc::Key::C).released {
            self.pov = FibonaciiExampleView::CpuMonitor;
        }

        if olc::get_key(olc::Key::M).released {
            self.pov = FibonaciiExampleView::MemoryMonitor(0x1000);
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
        olc::fill_circle(3, this_y, 2, olc::GREEN);

        // Separator
        olc::draw_line(0, 190, 260, 190, olc::WHITE);

        // Monitor
        let regs = self.cpu.regset();
        olc::draw_string(20, 200, &format!("A = {:#4x?}    SP = {:#4x?}", regs.accumulator(), regs.stk_ptr()), olc::WHITE)?;
        olc::draw_string(20, 210, &format!("X = {:#4x?}    PC = {:#6x?}", regs.x_index(), regs.prog_counter()), olc::WHITE)?;
        olc::draw_string(20, 220, &format!("Y = {:#4x?}    PS = {:#4x?}", regs.y_index(), regs.status()), olc::WHITE)?;

        // Separator
        olc::draw_line(0, 240, 260, 240, olc::DARK_CYAN);
        olc::draw_string(66, 245, &format!("CPU Monitor"), olc::DARK_CYAN);

        Ok(())
    }

    fn draw_mem(&mut self, begin_address: Address) -> Result<(), olc::Error> {
        let mut x = 50;
        let mut y = 10;

        olc::draw_string(10, 10, &format!("{:#6x?}:   ", begin_address), olc::YELLOW)?;
        for address in begin_address..begin_address + 80 {

            if address % 4 == 0 && address != begin_address {
                y += 10;
                x = 50;
                olc::draw_string(10, y, &format!("{:#6x?}:   ", address), olc::YELLOW)?;
            }

            let data = self.cpu.read_word(address);
            olc::draw_string(x, y, &format!("{:#6x?}", data), olc::WHITE)?;

            x += 35;
        }

        Ok(())
    }

}

fn main() {
    let mut fib_program = FibonacciExample::new("fib.bin");

    olc::start("Fibonacci example", &mut fib_program, 220, 260, 2, 2).unwrap();
}
