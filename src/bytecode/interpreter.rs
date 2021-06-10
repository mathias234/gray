use super::register::Register;
use crate::bytecode::code_block::CodeBlock;

pub struct ExecutionContext {
    accumulator: i64,
    registers: Vec<i64>,
}

impl ExecutionContext {
    pub fn new() -> ExecutionContext {
        ExecutionContext {
            accumulator: 0,
            registers: Vec::new(),
        }
    }

    pub fn set_accumulator(&mut self, value: i64) {
        self.accumulator = value;
    }
    pub fn get_accumulator(&self) -> i64 {
        self.accumulator
    }

    pub fn set_register(&mut self, register: &Register, value: i64) {
        while register.index >= self.registers.len() {
            self.registers.push(0);
        }
        self.registers[register.index] = value;
    }

    pub fn get_register(&self, register: &Register) -> i64 {
        self.registers[register.index]
    }
}

pub struct Interpreter<'a> {
    block: &'a mut CodeBlock,
    execution_context: ExecutionContext,
    pc: usize,
}

impl Interpreter<'_> {
    pub fn new(block: &mut CodeBlock) -> Interpreter {
        Interpreter {
            block,
            execution_context: ExecutionContext::new(),
            pc: 0,
        }
    }

    pub fn run(&mut self) {
        let instructions = self.block.get_instructions();
        let len = instructions.len();
        while self.pc < len {
            let ins = &instructions[self.pc];
            println!("Executing {}", ins.to_string());
            ins.execute(&mut self.execution_context);
            self.pc += 1;
        }

        println!("Final Registers:");
        let mut idx = 0;
        for reg in &self.execution_context.registers {
            println!("[{}] {}", idx, reg);
            idx += 1;
        }
        println!("[ACCUM] {}", self.execution_context.accumulator)
    }

}
