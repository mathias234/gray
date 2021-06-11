use super::register::Register;
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::label::Label;

pub struct ExecutionContext {
    accumulator: i64,
    registers: Vec<i64>,
    jump_target: Option<Label>,
}


impl ExecutionContext {
    pub fn new() -> ExecutionContext {
        ExecutionContext {
            accumulator: 0,
            registers: Vec::new(),
            jump_target: None,
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

    pub fn set_jump_target(&mut self, label: &Label) {
        self.jump_target = Some(*label);
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
        println!("Running code");

        let mut idx = 0;
        for ins in self.block.get_instructions() {
            println!("[{:04}] {}", idx, ins.to_string());
            idx += 1;
        }


        let instructions = self.block.get_instructions();
        let len = instructions.len();
        while self.pc < len {
            let ins = &instructions[self.pc];
            //println!("Executing {}", ins.to_string());
            ins.execute(&mut self.execution_context);

            if self.execution_context.jump_target.is_some() {
                self.pc = self.execution_context.jump_target.unwrap().position;
                self.execution_context.jump_target = None;
            } else {
                self.pc += 1;
            }
        }

        println!("Final Registers");
        let mut idx = 0;
        for reg in &self.execution_context.registers {
            println!("[{:04}] {}", idx, reg);
            idx += 1;
        }
        println!("[ACCU] {}", self.execution_context.accumulator)
    }
}
