use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::label::Label;
use crate::bytecode::instructions::other::Instruction;
use crate::bytecode::register::Register;

pub struct Generator {
    register_index: usize,
    pub block: CodeBlock,
}

impl Generator {
    pub fn new() -> Generator {
        Generator {
            register_index: 0,
            block: CodeBlock::new(),
        }
    }

    pub fn next_free_register(&mut self) -> Register {
        let result = Register::new(self.register_index);
        self.register_index += 1;
        result
    }

    pub fn make_label(&self) -> Label {
        let pos = self.block.get_instructions().len();
        Label::new(pos + 1)
    }

    pub fn emit(&mut self, instruction: Box<dyn Instruction>)  {
        self.block.add_instruction(instruction);
    }

    pub fn emit_at(&mut self, instruction: Box<dyn Instruction>, label: &Label) {
        self.block.add_instruction_at(instruction, label);
    }
}
