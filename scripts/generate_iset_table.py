from shutil import copyfile

HASHMAP_DECLARATION =\
    """
lazy_static! {
static ref DECODING_TABLE: HashMap<u16, Instruction> = hashmap! {
    """

HASHMAP_DECLARATION_END=\
    """
};
}
    """

RUST_INCLUDES=\
    """
use super::mos6502::{mos6502_addressing_modes::AddrMode, *};
use crate::instr;
use lazy_static::lazy_static;
use std::collections::HashMap;

    """

DATA_DIR="data"
RES_DIR="autogen"

SAVE_FILE_TMP = f"{RES_DIR}/iset_dectable.rs"
SAVE_FILE_DUP = "../src/cpu/mos6502_dt_auto_gen.rs"
LOAD_FILE = f"{DATA_DIR}/full_info.csv"
ADDR_MODE_TRANSLATION_TABLE = {
    'ACC': 'Accumulator',
    'IMM': 'Immediate',
    'IMP': 'Implied',
    'REL': 'Relative',
    'ABS': 'Absolute',
    'ZP': 'ZeroPage',
    'ZPX': 'ZeroPageX',
    'ZPY': 'ZeroPageY',
    'ABSX': 'IndexedX',
    'ABSY': 'IndexedY',
    'IND': 'Indirect',
    'INDX': 'IndexedIndirect',
    'INDY': 'IndirectIndexed',
}


def entry_from(opcode, mnemonic, addressing_mode, cycles, _bytes):
    addressing_mode = ADDR_MODE_TRANSLATION_TABLE[addressing_mode]
    func = mnemonic.lower()

    rv = ""
    if '/' in cycles:
        cycles = cycles.split('/')
        cycles_count = cycles[0]
        rv = f'\t// Can also take {cycles[1]} number of cycles.\n'
    else:
        cycles_count = int(cycles)

    rv += f'\t{opcode} => instr("{mnemonic}", {opcode}, Cpu::{func}, AddrMode::{addressing_mode}, {cycles_count}, {_bytes})' \
          f',\n\n'
    return rv


def main():
    f_save = open(SAVE_FILE_TMP, "w+")

    f_save.write(RUST_INCLUDES)
    f_save.write(HASHMAP_DECLARATION)

    with open(LOAD_FILE, "r") as f_load:
        lines = f_load.readlines()
        # Skip first line
        lines = iter(lines)
        next(lines)

        for line in lines:
            if line == '\n':
                continue
            # print(line)
            opcode, mnemonic, addr_mode_untranslated, b, cycles, flags = line.split(',')
            f_save.write(entry_from(
                opcode, mnemonic, addr_mode_untranslated, cycles, b
            ))

    f_save.write(HASHMAP_DECLARATION_END + '\n')
    f_save.close()

    copyfile(SAVE_FILE_TMP, SAVE_FILE_DUP)


if __name__ == "__main__":
    main()
