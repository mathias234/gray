pub use crate::{
    bytecode::{
        register::Register,
        label::Label,
        code_block::CodeBlock,
    },
    interpreter::{value::Value},
};

use std::time::Instant;

#[derive(Clone)]
pub struct ExecutionContext {
    accumulator: Value,
    registers: Vec<Value>,
    block_arguments: Vec<Value>,
    jump_target: Option<Label>,

    call_block_id: Option<usize>,
    call_arguments: Option<Vec<Register>>,
    call_return: bool,
}


impl ExecutionContext {
    pub fn new() -> ExecutionContext {
        ExecutionContext {
            accumulator: Value::from_i32(0),
            registers: Vec::new(),
            block_arguments: Vec::new(),
            jump_target: None,
            call_block_id: None,
            call_arguments: None,
            call_return: false,
        }
    }

    pub fn set_accumulator(&mut self, value: Value) {
        self.accumulator = value;
    }
    pub fn get_accumulator(&self) -> Value {
        self.accumulator
    }

    pub fn set_register(&mut self, register: &Register, value: Value) {
        while register.index >= self.registers.len() {
            self.registers.push(Value::from_i32(0));
        }
        self.registers[register.index] = value;
    }

    pub fn get_register(&self, register: &Register) -> Value {
        self.registers[register.index]
    }

    pub fn get_argument(&self, arg: usize) -> Value { self.block_arguments[arg] }

    pub fn set_jump_target(&mut self, label: &Label) {
        self.jump_target = Some(*label);
    }

    pub fn set_call(&mut self, block_id: usize) {
        self.call_block_id = Some(block_id)
    }
    pub fn set_call_arguments(&mut self, args: Option<Vec<Register>>) { self.call_arguments = args; }
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

        println!("Starting execution");

        let mut len = self.blocks[self.active_block].get_instructions().len();

        let now = Instant::now();

        while self.pc <= len {
            let active_block = &self.blocks[self.active_block];
            len = active_block.get_instructions().len();

            let instructions = active_block.get_instructions();
            let ins = &instructions[self.pc];
            //println!("Executing [{}]{}", self.active_block, ins.to_string());
            ins.execute(&mut self.execution_context);
            //self.dump();

            if self.execution_context.jump_target.is_some() {
                //println!("Jumping to {}", self.execution_context.jump_target.unwrap().position);
                self.pc = self.execution_context.jump_target.unwrap().position;
                self.execution_context.jump_target = None;
            } else if self.execution_context.call_block_id.is_some() {
                let call_block_id = self.execution_context.call_block_id.unwrap();
                self.execution_context.call_block_id = None;

                //println!("Calling block {}", call_block_id);
                let current_frame = StackFrame {
                    pc: self.pc,
                    execution_context: self.execution_context.clone(),
                    active_block: self.active_block,
                };

                self.call_stack.push(current_frame);

                self.active_block = call_block_id;

                let call_args = self.execution_context.call_arguments.clone();
                let mut block_args = Vec::new();

                if call_args.is_some() {
                    for call_arg in &call_args.unwrap() {
                        block_args.push(self.execution_context.get_register(call_arg));
                    }
                }

                self.execution_context = ExecutionContext::new();

                self.execution_context.block_arguments = block_args;
                self.pc = 0;
            } else if self.execution_context.call_return {
                self.execution_context.call_return = false;

                let last_frame = self.call_stack.pop();

                if last_frame.is_none() {
                    break;
                }

                let last_frame = last_frame.unwrap();

                //println!("Returning from block {} to block {}", self.active_block, last_frame.active_block);

                //self.dump();

                self.active_block = last_frame.active_block;
                self.execution_context = last_frame.execution_context;
                self.pc = last_frame.pc + 1;
            } else {
                //println!("Continue");
                self.pc += 1;
            }
        }

        println! {"Execution took {}ms", now.elapsed().as_millis()}
    }

    pub fn dump(&self) {
        println!("Dump of block {}'s Execution Context", self.active_block);
        println!("\tBlock Arguments");
        let mut idx = 0;
        for reg in &self.execution_context.block_arguments {
            println!("\t\t[{:04}] {}", idx, *reg);
            idx += 1;
        }
        println!("\tRegisters");
        let mut idx = 0;
        for reg in &self.execution_context.registers {
            println!("\t\t[{:04}] {}", idx, *reg);
            idx += 1;
        }

        println!("\t[ACCU] {}", self.execution_context.accumulator)
    }
}
