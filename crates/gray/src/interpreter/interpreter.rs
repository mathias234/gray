use crate::{
    bytecode::{code_block::CodeBlock, label::Label, register::Register},
    error_printer,
    interpreter::value::Value,
};

use crate::bytecode::code_block::CodeSegment;
use crate::compiler::compiler::NativeFunction;
use crate::interpreter::function_pointer::FunctionArgs;
use crate::interpreter::value::DataValue;
use std::cell::Cell;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

pub type VariableHandle = usize;

#[derive(Clone)]
pub struct Scope {
    variables: Vec<Value>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: Vec::new(),
        }
    }
}

#[derive(Clone)]
pub struct BreakContinueScope {
    break_label: Label,
    continue_label: Label,
}

impl BreakContinueScope {
    pub fn new(break_label: Label, continue_label: Label) -> BreakContinueScope {
        BreakContinueScope {
            break_label,
            continue_label,
        }
    }
}

#[derive(Clone)]
pub struct ExecutionContext {
    accumulator: Value,
    registers: Vec<Value>,
    block_arguments: Vec<Value>,
    jump_target: Option<Label>,

    call_block_id: Option<VariableHandle>,
    call_arguments: Option<Vec<Register>>,
    call_return: bool,

    scope_stack: Vec<Scope>,

    should_break: bool,
    should_continue: bool,
    break_continue_scope_stack: Vec<BreakContinueScope>,

    errored: Cell<bool>,
    code_text: Rc<String>,
    code_segment: CodeSegment,
}

impl ExecutionContext {
    pub fn new(code_text: Rc<String>) -> ExecutionContext {
        ExecutionContext {
            accumulator: Value::from_i64(0),
            registers: Vec::new(),
            block_arguments: Vec::new(),
            jump_target: None,
            call_block_id: None,
            call_arguments: None,
            call_return: false,
            scope_stack: vec![Scope::new()],
            should_break: false,
            should_continue: false,
            break_continue_scope_stack: Vec::new(),

            code_segment: CodeSegment::new(1, 1, 1, 1),
            code_text,
            errored: Cell::from(false),
        }
    }

    pub fn set_accumulator(&mut self, value: Value) {
        self.accumulator = value;
    }
    pub fn get_accumulator(&self) -> Value {
        self.accumulator.clone()
    }

    pub fn set_register(&mut self, register: &Register, value: Value) {
        while register.index >= self.registers.len() {
            self.registers.push(Value::from_i64(0));
        }
        self.registers[register.index] = value;
    }

    pub fn get_register(&self, register: &Register) -> Value {
        self.registers[register.index].clone()
    }

    pub fn get_argument(&self, arg: usize) -> Value {
        if self.block_arguments.len() <= arg {
            return self.throw_error(&format!("Function expected at least {} arguments", arg + 1));
        }
        self.block_arguments[arg].clone()
    }

    pub fn get_argument_count(&self) -> usize {
        self.block_arguments.len()
    }

    pub fn set_jump_target(&mut self, label: &Label) {
        self.jump_target = Some(*label);
    }

    pub fn set_call(&mut self, block_id: VariableHandle) {
        self.call_block_id = Some(block_id)
    }
    pub fn set_call_arguments(&mut self, args: Option<Vec<Register>>) {
        self.call_arguments = args;
    }
    pub fn set_return(&mut self) {
        self.call_return = true;
    }

    pub fn declare_variable(&mut self, variable: VariableHandle, value: &Value) {
        let scope_stack = &mut self.scope_stack[0];
        let var_count = scope_stack.variables.len();
        if var_count <= variable {
            for _ in var_count..variable + 1 {
                scope_stack.variables.push(Value::from_undefined());
            }
        }

        scope_stack.variables[variable] = value.clone();
    }

    pub fn set_variable(&mut self, variable: VariableHandle, value: &Value) {
        let mut found_variable = false;
        for scope in &mut self.scope_stack {
            if scope.variables.len() > variable && !scope.variables[variable].is_undefined() {
                scope.variables[variable] = value.clone();
                found_variable = true;
                break;
            }
        }

        if !found_variable {
            self.throw_error(&format!("Failed to find variable"));
        }
    }

    pub fn get_variable_no_error(&self, variable: VariableHandle) -> Option<Value> {
        for scope in &self.scope_stack {
            if scope.variables.len() <= variable {
                continue;
            }

            let variable = &scope.variables[variable];

            if !variable.is_undefined() {
                return Some(variable.clone());
            }
        }

        None
    }

    pub fn get_variable(&self, variable: VariableHandle) -> Value {
        let variable = self.get_variable_no_error(variable);

        if let Some(variable) = variable {
            return variable;
        }

        self.throw_error(&format!("Failed to find variable"));
        Value::from_i64(-1)
    }

    pub fn push_scope(&mut self) {
        self.scope_stack.insert(0, Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scope_stack.remove(0);
    }

    pub fn push_break_continue_scope(&mut self, break_label: Label, continue_label: Label) {
        self.break_continue_scope_stack
            .insert(0, BreakContinueScope::new(break_label, continue_label));
    }

    pub fn pop_break_continue_scope(&mut self) -> BreakContinueScope {
        self.break_continue_scope_stack.remove(0)
    }

    pub fn get_top_break_continue_scope(&self) -> &BreakContinueScope {
        &self.break_continue_scope_stack[0]
    }

    pub fn set_break(&mut self) {
        self.should_break = true;
    }
    pub fn set_continue(&mut self) {
        self.should_continue = true;
    }

    pub fn throw_error(&self, message: &str) -> Value {
        self.errored.set(true);

        println!("Runtime Error!");
        println!("---------------------------------");
        let lines: Vec<&str> = self.code_text.split('\n').collect();
        let line_count = self.code_segment.end_y - self.code_segment.start_y;

        if line_count == 0 {
            error_printer::print_error_line(
                lines[self.code_segment.start_y - 1],
                self.code_segment.start_y,
                self.code_segment.start_x,
                self.code_segment.end_x,
            );
        } else {
            for line in self.code_segment.start_y - 1..self.code_segment.end_y - 1 {
                let start_x = 0;
                let end_x = lines[line].len();

                error_printer::print_error_line(lines[line], line + 1, start_x, end_x);
            }
        }

        println!(" {}", message);
        Value::from_i64(-1)
    }
}

pub struct StackFrame {
    pc: usize,
    execution_context: ExecutionContext,
    active_block: String,
    caller_segment: CodeSegment,
}

pub struct Interpreter<'interp> {
    active_block: String,
    active_code_block: Option<&'interp CodeBlock>,
    execution_context: ExecutionContext,
    pc: usize,
    blocks: HashMap<String, CodeBlock>,

    call_stack: Vec<StackFrame>,

    native_functions: HashMap<String, NativeFunction>,

    code_text: Rc<String>,
}

impl<'interp> Interpreter<'interp> {
    pub fn new(
        blocks: HashMap<String, CodeBlock>,
        code_text: Rc<String>,
        native_functions: Vec<NativeFunction>,
    ) -> Interpreter<'interp> {
        let mut interp = Interpreter {
            active_block: String::from(""),
            active_code_block: None,
            execution_context: ExecutionContext::new(code_text.clone()),
            pc: 0,
            blocks,
            call_stack: Vec::new(),
            native_functions: HashMap::new(),
            code_text,
        };

        for func in native_functions {
            interp.set_native_function(func);
        }

        interp
    }

    pub fn run(&'interp mut self, start_block: Option<String>) -> Value {
        self.active_block = match start_block {
            Some(start_block) => start_block,
            None => String::from("ProgramMain"),
        };

        #[cfg(debug_assertions)]
        {
            println!("Compiled code");
            for (name, block) in &self.blocks {
                println!("\tBlock {}, Capture locals: {}", name, block.capture_locals);
                let mut idx = 0;

                let mut indent = 2;
                for ins in block.get_instructions() {
                    if ins.to_string() == String::from("PopScope") {
                        indent -= 1;
                    }

                    for _ in 0..indent {
                        print!("\t");
                    }

                    println!("// Segment ${:?}", block.code_mapping[idx]);

                    for _ in 0..indent {
                        print!("\t");
                    }
                    println!("[{:04}] {}", idx, ins.to_string());
                    println!();
                    if ins.to_string() == String::from("PushScope") {
                        indent += 1;
                    }
                    idx += 1;
                }

                println!();
            }
        }

        self.active_code_block = Some(&self.blocks[&self.active_block]);

        let mut len = self.active_code_block.unwrap().get_instructions().len();

        while self.pc < len {
            let active_block = self.active_code_block.unwrap();

            let instructions = active_block.get_instructions();
            let ins = &instructions[self.pc];
            //println!("Executing [{}][{}]{}", self.active_block, self.pc, ins.to_string());
            self.execution_context.code_segment =
                self.active_code_block.unwrap().code_mapping[self.pc];
            ins.execute(&mut self.execution_context);

            if self.execution_context.errored.get() {
                // The context errored, we will then stop the execution of the interpreter
                if self.call_stack.len() != 0 {
                    println!("Stacktrace");

                    let mut i = (self.call_stack.len() - 1) as isize;

                    while i >= 0 {
                        let frame = &self.call_stack[i as usize];
                        let lines: Vec<&str> = self.code_text.split('\n').collect();

                        error_printer::print_error_line(
                            lines[frame.caller_segment.start_y - 1],
                            frame.caller_segment.start_y,
                            frame.caller_segment.start_x,
                            frame.caller_segment.end_x,
                        );
                        println!();

                        i -= 1;
                    }
                }

                println!("Registers");
                self.dump();

                panic!();
            }

            //self.dump();

            if self.execution_context.jump_target.is_some() {
                //println!("Jumping to {}", self.execution_context.jump_target.unwrap().position);
                self.pc = self.execution_context.jump_target.unwrap().position;
                self.execution_context.jump_target = None;
            } else if self.execution_context.call_block_id.is_some() {
                let call_block_id = self.execution_context.call_block_id.clone().unwrap();
                self.execution_context.call_block_id = None;

                /*
                println!("Calling block {} with arguments", call_block_id);
                let mut idx = 0;
                for reg in self.execution_context.call_arguments.as_ref().unwrap() {
                    println!("\t[{:04}] {}", idx, self.execution_context.registers[reg.index]);
                    idx += 1;
                }
                */

                let call_args = &self.execution_context.call_arguments;
                let mut block_args = Vec::new();

                if call_args.is_some() {
                    for call_arg in call_args.as_ref().unwrap() {
                        block_args.push(self.execution_context.get_register(call_arg));
                    }
                }

                let variable = self.execution_context.get_variable_no_error(call_block_id);

                if variable.is_none() {
                    self.execution_context
                        .throw_error("Failed to find variable");
                    continue;
                }

                let handle = match variable.unwrap().get_data_value() {
                    DataValue::FunctionPointer(handle) => handle.clone(),
                    DataValue::StructDef(def) => {
                        let obj = def.create_object();
                        let handle = obj.get(Rc::from("__InternalConstructor__".to_string()));
                        if handle.is_none() {
                            self.execution_context
                                .throw_error(&format!("Could not find constructor on {:?}", def));

                            continue;
                        }

                        block_args.insert(0, Value::from_object(obj));

                        match handle.unwrap().get_data_value() {
                            DataValue::FunctionPointer(handle) => handle.clone(),
                            v => {
                                self.execution_context.throw_error(&format!(
                                    "Expected value to be a function handle it is {:?}",
                                    v
                                ));
                                continue;
                            }
                        }
                    }
                    v => {
                        self.execution_context.throw_error(&format!(
                            "Expected value to be a function handle it is {:?}",
                            v
                        ));
                        continue;
                    }
                };

                let block_to_call = self.blocks.get(&*handle);

                if block_to_call.is_none() {
                    let native_function = self.native_functions.get(&*handle);
                    match native_function {
                        Some(func) => {
                            let returned_value =
                                func.call(&self.execution_context, FunctionArgs::new(block_args));
                            self.execution_context.set_accumulator(returned_value);
                            self.pc += 1;
                            continue;
                        }
                        None => self
                            .execution_context
                            .throw_error(&format!("Unable to find function {:?}", call_block_id)),
                    };
                }

                let old_context = mem::replace(
                    &mut self.execution_context,
                    ExecutionContext::new(self.code_text.clone()),
                );

                let block_to_call = block_to_call.unwrap();

                if block_to_call.capture_locals {
                    let mut variables = Vec::new();
                    for scope in &old_context.scope_stack {
                        for _ in variables.len()..scope.variables.len() {
                            variables.push(Value::from_undefined())
                        }

                        for i in 0..scope.variables.len() {
                            if variables[i].is_undefined() && scope.variables[i].is_undefined() {
                                continue;
                            }

                            variables[i] = scope.variables[i].clone();
                        }
                    }

                    self.execution_context.scope_stack[0].variables = variables;
                }

                let current_frame = StackFrame {
                    pc: self.pc,
                    execution_context: old_context,
                    active_block: self.active_block.clone(),
                    caller_segment: self.execution_context.code_segment,
                };

                self.call_stack.push(current_frame);

                self.active_block = handle.to_string();
                self.active_code_block = Some(block_to_call);

                len = self.active_code_block.unwrap().get_instructions().len();

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

                // Store the last value of the returned block in the accumulator
                let accumulator = self.execution_context.get_accumulator();

                self.active_block = last_frame.active_block;
                self.active_code_block = self.blocks.get(&self.active_block);
                if self.active_code_block.is_none() {
                    panic!(
                        "Error in return could not find previous function `{}`",
                        self.active_block
                    );
                }

                len = self.active_code_block.unwrap().get_instructions().len();
                self.execution_context = last_frame.execution_context;

                // Put the value back in the accumulator into the current blocks accumulator
                self.execution_context.set_accumulator(accumulator);
                self.pc = last_frame.pc + 1;
            } else if self.execution_context.should_break {
                // break
                let break_scope = self.execution_context.get_top_break_continue_scope();
                self.pc = break_scope.break_label.position;
                self.execution_context.should_break = false;
            } else if self.execution_context.should_continue {
                // continue
                let continue_scope = self.execution_context.get_top_break_continue_scope();
                self.pc = continue_scope.continue_label.position;
                self.execution_context.should_continue = false;
            } else {
                //println!("Continue");
                self.pc += 1;
            }
        }

        //self.dump();
        self.get_last_accumulator_value()
    }

    fn set_native_function(&mut self, function: NativeFunction) {
        let mut full_name = String::new();
        for n in &function.namespace {
            full_name += &format!("{}::", n);
        }
        full_name += &function.name;
        self.native_functions.insert(full_name, function);
    }

    pub fn get_last_accumulator_value(&self) -> Value {
        self.execution_context.accumulator.clone()
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
