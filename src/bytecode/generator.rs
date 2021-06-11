use super::{instructions::Instruction, register::Register};
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::label::Label;

pub struct Generator {
    register_index: usize,
    block: CodeBlock,
}

impl Generator {
    pub fn new() -> Generator {
        Generator {
            register_index: 0,
            block: CodeBlock::new(),
        }
    }

    pub fn get_block(&mut self) -> &mut CodeBlock {
        &mut self.block
    }

    pub fn next_free_register(&mut self) -> Register {
        let result = Register::new(self.register_index);
        self.register_index += 1;
        result
    }

    pub fn make_label(&self) -> Label {
        let pos = self.block.get_instructions().len();
        Label::new(pos)
    }

    pub fn emit(&mut self, instruction: Box<dyn Instruction>) {
        self.block.add_instruction(instruction);
    }
}
