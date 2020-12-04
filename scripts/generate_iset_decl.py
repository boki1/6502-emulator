FUNC_TEMPLATE_FRONT = "pub fn"
FUNC_TEMPLATE_BACK = "(&mut self) {"
#  UNIMPLEMENTED = "\n\tunimplemented!();\n}"

def unimplemented(mnemon):
    str_ = "\n"
    str_ += f"\t\tinfo!(\"{mnemon}-ing...\");\n"
    str_ += "\t\tinfo!(\"{:?}\", self.fetched);\n"
    return str_

DATA_DIR="data"
RES_DIR="autogen"

SAVE_FILE_TMP = f"{RES_DIR}/iset_functions.rs"
SAVE_FILE_DUP = "../src/cpu/mos6502_iset_auto_gen.rs"
LOAD_FILE = f"{DATA_DIR}/instr_set_proto.txt"

RUST_INCLUDES=\
    """
use log::*;
use super::mos6502::Cpu;
    """

from shutil import copyfile

if __name__ == '__main__':
    f_save = open(SAVE_FILE_TMP, "w+")

    f_save.write(RUST_INCLUDES)

    with open(LOAD_FILE, "r") as f_load:
        f_save.write("impl Cpu {\n")

        for mnemonic in f_load.readlines():
            m = mnemonic.strip('\n').lower()
            f_save.write(f'\t{FUNC_TEMPLATE_FRONT} {m}{FUNC_TEMPLATE_BACK}{unimplemented(m)}' + '\t}\n\n')

        f_save.write("}")

    f_save.close()

    copyfile(SAVE_FILE_TMP, SAVE_FILE_DUP)

