use crate::bytecode::code_block::{CodeBlock, CodeSegment};
use crate::bytecode::label::Label;
use crate::bytecode::instructions::other::Instruction;
use crate::bytecode::register::Register;
use crate::bytecode::instructions::object::NotAnInstruction;

pub struct Generator {
    register_index: usize,
    released_registers: Vec<Register>,
    pub block: CodeBlock,
}

impl Generator {
    pub fn new() -> Generator {
        Generator {
            register_index: 0,
            released_registers: Vec::new(),
            block: CodeBlock::new(),
        }
    }

    pub fn next_free_register(&mut self) -> Register {
        if self.released_registers.len() > 0 {
            let register = self.released_registers.pop().unwrap();
            return register;
        }

        let result = Register::new(self.register_index);
        self.register_index += 1;
        result
    }

    pub fn release_register(&mut self, register: Register) {
        self.released_registers.push(register);
    }

    pub fn make_label(&self) -> Label {
        let pos = self.block.get_instructions().len();
        Label::new(pos)
    }

    pub fn make_instruction_holder(&mut self) -> Label {
        let label = self.make_label();

        self.emit(NotAnInstruction::new_boxed(), CodeSegment::new(0, 0, 0, 0));

        return label;
    }

    pub fn emit(&mut self, instruction: Box<dyn Instruction>, segment: CodeSegment) {
        self.block.add_instruction(instruction, segment);
    }

    pub fn emit_at(&mut self, instruction: Box<dyn Instruction>, label: &Label, segment: CodeSegment) {
        self.block.set_instruction_at(instruction, label, segment);
    }
}
