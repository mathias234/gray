use std::fmt::Write as FmtWrite;
use crate::{
    bytecode::{register::Register},
    interpreter::{interpreter::{ExecutionContext, VariableHandle}, value::Value},
};
use crate::bytecode::label::Label;

pub trait Instruction {
    fn execute(&self, context: &mut ExecutionContext);
    fn to_string(&self) -> String;
}

pub struct LoadImmediate {
    value: Value,
}

#[allow(dead_code)]
impl LoadImmediate {
    pub fn new_boxed(value: Value) -> Box<LoadImmediate> {
        Box::new(LoadImmediate { value })
    }
}

pub struct LoadRegister {
    register: Register,
}

#[allow(dead_code)]
impl LoadRegister {
    pub fn new_boxed(register: Register) -> Box<LoadRegister> {
        Box::new(LoadRegister { register })
    }
}

pub struct LoadArgument {
    argument: usize,
}

#[allow(dead_code)]
impl LoadArgument {
    pub fn new_boxed(argument: usize) -> Box<LoadArgument> {
        Box::new(LoadArgument {
            argument
        })
    }
}

pub struct Store {
    register: Register,
}

#[allow(dead_code)]
impl Store {
    pub fn new_boxed(register: Register) -> Box<Store> {
        Box::new(Store { register })
    }
}

pub struct Call {
    block_id: VariableHandle,
    arguments: Vec<Register>,
}

impl Call {
    pub fn new_boxed(block_id: VariableHandle, args: Option<Vec<Register>>) -> Box<Call> {
        if args.is_some() {
            return Box::new(Call { block_id, arguments: args.unwrap() });
        }

        Box::new(Call { block_id, arguments: Vec::new() })
    }
}

pub struct Return {}

impl Return {
    pub fn new_boxed() -> Box<Return> {
        Box::new(Return {})
    }
}

pub struct DeclareVariable {
    variable: VariableHandle,
}

impl DeclareVariable {
    pub fn new_boxed(variable: VariableHandle) -> Box<DeclareVariable> {
        Box::new(DeclareVariable { variable })
    }
}

pub struct SetVariable {
    variable: VariableHandle,
}

impl SetVariable {
    pub fn new_boxed(variable: VariableHandle) -> Box<SetVariable> {
        Box::new(SetVariable { variable })
    }
}


pub struct GetVariable {
    variable: VariableHandle,
}

impl GetVariable {
    pub fn new_boxed(variable: VariableHandle) -> Box<GetVariable> {
        Box::new(GetVariable { variable })
    }
}

pub struct PushScope {}

impl PushScope {
    pub fn new_boxed() -> Box<PushScope> {
        Box::new(PushScope {})
    }
}

pub struct PopScope {}

impl PopScope {
    pub fn new_boxed() -> Box<PopScope> {
        Box::new(PopScope {})
    }
}


pub struct PushBreakContinueScope {
    continue_label: Label,
    break_label: Label,
}

impl PushBreakContinueScope {
    pub fn new_boxed(break_label: Label, continue_label: Label) -> Box<PushBreakContinueScope> {
        Box::new(PushBreakContinueScope { break_label, continue_label })
    }
}

pub struct PopBreakContinueScope {}

impl PopBreakContinueScope {
    pub fn new_boxed() -> Box<PopBreakContinueScope> {
        Box::new(PopBreakContinueScope {})
    }
}

pub struct Break {}

impl Break {
    pub fn new_boxed() -> Box<Break> {
        Box::new(Break {})
    }
}

pub struct Continue {}

impl Continue {
    pub fn new_boxed() -> Box<Continue> {
        Box::new(Continue {})
    }
}

// Instruction implementations

impl Instruction for LoadImmediate {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_accumulator(self.value.clone());
    }

    fn to_string(&self) -> String {
        format!("LoadImmediate value: {}", self.value)
    }
}

impl Instruction for LoadRegister {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_accumulator(context.get_register(&self.register));
    }

    fn to_string(&self) -> String {
        format!("LoadRegister value: {}", self.register)
    }
}

impl Instruction for LoadArgument {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_accumulator(context.get_argument(self.argument));
    }

    fn to_string(&self) -> String {
        format!("LoadArgument argument: {}", self.argument)
    }
}

impl Instruction for Store {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_register(&self.register, context.get_accumulator());
    }

    fn to_string(&self) -> String {
        format!("Store {}", self.register)
    }
}

impl Instruction for Call {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_call_arguments(Some(self.arguments.clone()));
        context.set_call(self.block_id);
    }

    fn to_string(&self) -> String {
        let mut writer = String::new();
        write!(&mut writer, "Call block {} Args: (", self.block_id).unwrap();
        let mut idx = 0;
        for a in &self.arguments {
            write!(&mut writer, "{}", a).unwrap();

            if idx < self.arguments.len() - 1 {
                write!(&mut writer, ", ").unwrap();
            }

            idx += 1;
        }

        write!(&mut writer, ")").unwrap();

        writer
    }
}

impl Instruction for Return {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_return();
    }

    fn to_string(&self) -> String { format!("Return") }
}

impl Instruction for DeclareVariable {
    fn execute(&self, context: &mut ExecutionContext) {
        context.declare_variable(self.variable, &context.get_accumulator());
    }

    fn to_string(&self) -> String {
        format!("DeclareVariable {}", self.variable)
    }
}

impl Instruction for SetVariable {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_variable(self.variable, &context.get_accumulator());
    }

    fn to_string(&self) -> String {
        format!("SetVariable {}", self.variable)
    }
}


impl Instruction for GetVariable {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_variable(self.variable);
        context.set_accumulator(value);
    }

    fn to_string(&self) -> String {
        format!("GetVariable {}", self.variable)
    }
}

impl Instruction for PushScope {
    fn execute(&self, context: &mut ExecutionContext) {
        context.push_scope()
    }

    fn to_string(&self) -> String {
        format!("PushScope")
    }
}

impl Instruction for PopScope {
    fn execute(&self, context: &mut ExecutionContext) {
        context.pop_scope()
    }

    fn to_string(&self) -> String {
        format!("PopScope")
    }
}


impl Instruction for PushBreakContinueScope {
    fn execute(&self, context: &mut ExecutionContext) {
        context.push_break_continue_scope(self.break_label, self.continue_label);
    }

    fn to_string(&self) -> String {
        format!("PushBreakContinueScope")
    }
}

impl Instruction for PopBreakContinueScope {
    fn execute(&self, context: &mut ExecutionContext) {
        context.pop_break_continue_scope();
    }

    fn to_string(&self) -> String {
        format!("PopBreakContinueScope")
    }
}

impl Instruction for Break {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_break();
    }

    fn to_string(&self) -> String {
        format!("Break")
    }
}

impl Instruction for Continue {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_continue();
    }

    fn to_string(&self) -> String {
        format!("Continue")
    }
}
