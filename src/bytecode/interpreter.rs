use super::register::Register;
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::label::Label;

#[derive(Clone)]
pub struct ExecutionContext {
    accumulator: i64,
    registers: Vec<i64>,
    jump_target: Option<Label>,

    call_block_id: Option<usize>,
    call_return: bool,
}


impl ExecutionContext {
    pub fn new() -> ExecutionContext {
        ExecutionContext {
            accumulator: 0,
            registers: Vec::new(),
            jump_target: None,
            call_block_id: None,
            call_return: false,
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

    pub fn set_call(&mut self, block_id: usize) {
        self.call_block_id = Some(block_id)
    }
    pub fn set_return(&mut self) {
        self.call_return = true;
    }
}

pub struct StackFrame {
    pc: usize,
    execution_context: ExecutionContext,
    active_block: usize,
}

pub struct Interpreter<'a> {
    active_block: usize,
    execution_context: ExecutionContext,
    pc: usize,
    blocks: Vec<&'a mut CodeBlock>,

    call_stack: Vec<StackFrame>,
}

impl Interpreter<'_> {
    pub fn new(blocks: Vec<&mut CodeBlock>) -> Interpreter {
        Interpreter {
            active_block: 0,
            execution_context: ExecutionContext::new(),
            pc: 0,
            blocks,
            call_stack: Vec::new(),
        }
    }

    pub fn run(&mut self) {
        println!("Compiled code");
        let mut block_idx = 0;
        for block in &self.blocks {
            println!("\tBlock {}", block_idx);
            let mut idx = 0;

            for ins in block.get_instructions() {
                println!("\t\t[{:04}] {}", idx, ins.to_string());
                idx += 1;
            }

            block_idx += 1;
            println!();
        }


        let active_block = &self.blocks[self.active_block];

        while self.pc <= active_block.get_instructions().len() {
            let active_block = &self.blocks[self.active_block];

            let instructions = active_block.get_instructions();
            let ins = &instructions[self.pc];
            //println!("Executing [{}]{}", self.active_block, ins.to_string());
            ins.execute(&mut self.execution_context);

            if self.execution_context.jump_target.is_some() {
                self.pc = self.execution_context.jump_target.unwrap().position;
                self.execution_context.jump_target = None;
            } else if self.execution_context.call_block_id.is_some() {
                let call_block_id = self.execution_context.call_block_id.unwrap();
                self.execution_context.call_block_id = None;

                println!("Calling block {}", call_block_id);
                let current_frame = StackFrame {
                    pc: self.pc,
                    execution_context: self.execution_context.clone(),
                    active_block: self.active_block,
                };

                self.call_stack.push(current_frame);

                self.active_block = call_block_id;
                self.execution_context = ExecutionContext::new();
                self.pc = 0;

            } else if self.execution_context.call_return {
                self.execution_context.call_return = false;

                let last_frame = self.call_stack.pop();

                if last_frame.is_none() {
                    break;
                }

                let last_frame = last_frame.unwrap();

                println!("Returning from block {} to block {}", self.active_block, last_frame.active_block);

                self.dump();

                self.active_block = last_frame.active_block;
                self.execution_context = last_frame.execution_context;
                self.pc = last_frame.pc + 1;

            } else {
                self.pc += 1;
            }
        }

        println!("Ran out of code to execute. Finished.");
        self.dump();
    }

    pub fn dump(&self) {
        println!("Dump of block {}'s registers", self.active_block);
        let mut idx = 0;
        for reg in &self.execution_context.registers {
            println!("[{:04}] {}", idx, reg);
            idx += 1;
        }
        println!("[ACCU] {}", self.execution_context.accumulator)
    }
}
