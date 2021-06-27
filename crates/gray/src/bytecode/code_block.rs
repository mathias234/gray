use crate::bytecode::label::Label;
use crate::bytecode::instructions::other::Instruction;

pub struct CodeBlock {
    instructions: Vec<Box<dyn Instruction>>
}

impl CodeBlock {
    pub fn new() -> CodeBlock {
        CodeBlock { instructions: Vec::new() }
    }

    pub fn add_instruction(&mut self, instruction: Box<dyn Instruction>) {
        self.instructions.push(instruction);
    }

    pub fn set_instruction_at(&mut self, instruction: Box<dyn Instruction>, label: &Label) {
        self.instructions[label.position] = instruction;
    }

    pub fn get_instructions(&self) -> &Vec<Box<dyn Instruction>> {
        &self.instructions
    }
}