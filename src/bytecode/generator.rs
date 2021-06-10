use super::{instructions::Instruction, register::Register};

pub struct Generator {
    register_index: usize,
}

impl Generator {
    pub fn new() -> Generator {
        Generator { register_index: 0 }
    }

    pub fn next_free_register(&mut self) -> Register {
        let result = Register::new(self.register_index);
        self.register_index += 1;
        result
    }

    pub fn emit(&self, instruction: &impl Instruction) {
        println!("{}", instruction.to_string());
    }
}
