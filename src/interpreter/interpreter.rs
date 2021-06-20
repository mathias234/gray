pub use crate::{
    bytecode::{
        register::Register,
        label::Label,
        code_block::CodeBlock,
    },
    interpreter::{value::Value},
};

use std::time::Instant;
use std::collections::HashMap;

#[derive(Clone)]
pub struct ExecutionContext {
    accumulator: Value,
    registers: Vec<Value>,
    block_arguments: Vec<Value>,
    jump_target: Option<Label>,

    call_block_id: Option<String>,
    call_arguments: Option<Vec<Register>>,
    call_return: bool,

    local_variables: HashMap<String, Value>,
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
            local_variables: HashMap::new()
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

    pub fn set_call(&mut self, block_id: &str) {
        self.call_block_id = Some(String::from(block_id))
    }
    pub fn set_call_arguments(&mut self, args: Option<Vec<Register>>) { self.call_arguments = args; }
    pub fn set_return(&mut self) {
        self.call_return = true;
    }

    pub fn set_variable(&mut self, variable: &String, value: Value) {
        self.local_variables.insert(variable.clone(), value);
    }
    pub fn get_variable(&self, variable: &String) -> Value {
        self.local_variables[variable]
    }
}

pub struct StackFrame {
    pc: usize,
    execution_context: ExecutionContext,
    active_block: String,
}

pub struct Interpreter {
    active_block: String,
    execution_context: ExecutionContext,
    pc: usize,
    blocks: HashMap<String, CodeBlock>,

    call_stack: Vec<StackFrame>,
}

impl Interpreter {
    pub fn new(blocks: HashMap<String, CodeBlock>) -> Interpreter {
        Interpreter {
            active_block: String::from("entry"),
            execution_context: ExecutionContext::new(),
            pc: 0,
            blocks,
            call_stack: Vec::new(),
        }
    }

    pub fn run(&mut self) {
        println!("Compiled code");
        for (name, block) in &self.blocks {
            println!("\tBlock {}", name);
            let mut idx = 0;

            for ins in block.get_instructions() {
                println!("\t\t[{:04}] {}", idx, ins.to_string());
                idx += 1;
            }

            println!();
        }

        println!("Starting execution");

        let mut len = self.blocks[&self.active_block].get_instructions().len();

        let now = Instant::now();

        while self.pc <= len {
            let active_block = &self.blocks[&self.active_block];
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
                let call_block_id= self.execution_context.call_block_id.clone().unwrap();
                self.execution_context.call_block_id = None;

                //println!("Calling block {}", call_block_id);

                let current_frame = StackFrame {
                    pc: self.pc,
                    execution_context: self.execution_context.clone(),
                    active_block: self.active_block.clone()
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

        //self.dump();

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

        println!("\tVariables");
        let mut idx = 0;
        for (variable_name, value) in &self.execution_context.local_variables {
            println!("\t\t[{:04}] {}: {}", idx, variable_name, value);
            idx += 1;
        }

        println!("\t[ACCU] {}", self.execution_context.accumulator)
    }
}
