# 6502 emulator

This is an emulator of the [MOS 6502 CPU](https://en.wikipedia.org/wiki/MOS_Technology_6502).

--------------------------------------

**Currently supported features**
- support for all legal opcodes
- support for all addressing modes
- emulates cycle-correctness
- easy integration with GUI monitor

--------------------------------------

**Future upgrades**
- illegal opcodes support
- more monitor customization options
- web integration

--------------------------------------

**In case you want to try it**
+ Since the technology of use is Rust you will need to have it installed. If you need help, check [here](https://www.rust-lang.org/) how to do that.
+ The next thing you will need is the source code itself. You can do that by either cloning the repository or downloading it. (_Note:_ For cloning the repository you will need to have `git` installed.) 
+ A set of example programs and usages have been added in the [`examples/`](https://github.com/boki1/6502-emulator/tree/dev/examples/) directory. You can pick any of them to try out.
+ Now you can just go with `cargo run` in the `src/` directory of the example.

Example:

```fish
# Clone the repository
git clone https://github.com/boki1/6502-emulator

# Choose the example you would like to try out
cd 6502-emulator/examples/fib/src

# Run it
cargo run

```

--------------------------------------

A few images...

A Fibonacci sequence calculator\
![Fibonacci](https://ibin.co/5ynmG0g2ucAS.png)

----------------

Greatest common divider euclidean algorithm\
![Greatest common divider](https://ibin.co/5ynmUgTr7QG0.png)
