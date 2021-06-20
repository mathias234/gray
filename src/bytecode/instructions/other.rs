use std::fmt::Write as FmtWrite;
use crate::{
    bytecode::{register::Register},
    interpreter::{interpreter::ExecutionContext, value::Value},
};
use std::rc::Rc;


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


pub struct CompareEq {
    register: Register,
}

#[allow(dead_code)]
impl CompareEq {
    pub fn new_boxed(register: Register) -> Box<CompareEq> {
        Box::new(CompareEq { register })
    }
}

pub struct CompareNotEq {
    register: Register,
}

#[allow(dead_code)]
impl CompareNotEq {
    pub fn new_boxed(register: Register) -> Box<CompareNotEq> {
        Box::new(CompareNotEq { register })
    }
}

pub struct CompareGreaterThan {
    register: Register,
}

#[allow(dead_code)]
impl CompareGreaterThan {
    pub fn new_boxed(register: Register) -> Box<CompareGreaterThan> {
        Box::new(CompareGreaterThan { register })
    }
}

pub struct CompareLessThan {
    register: Register,
}

#[allow(dead_code)]
impl CompareLessThan {
    pub fn new_boxed(register: Register) -> Box<CompareLessThan> {
        Box::new(CompareLessThan { register })
    }
}


pub struct Call {
    block_id: Rc<str>,
    arguments: Vec<Register>,
}

impl Call {
    pub fn new_boxed(block_id: &str, args: Option<Vec<Register>>) -> Box<Call> {
        if args.is_some() {
            return Box::new(Call { block_id: Rc::from(block_id), arguments: args.unwrap() });
        }

        Box::new(Call { block_id: Rc::from(block_id), arguments: Vec::new() })
    }
}

pub struct Return {}

impl Return {
    pub fn new_boxed() -> Box<Return> {
        Box::new(Return {})
    }
}

pub struct SetVariable {
    variable: String,
}

impl SetVariable {
    pub fn new_boxed(variable: String) -> Box<SetVariable> {
        Box::new(SetVariable { variable })
    }
}

pub struct GetVariable {
    variable: String,
}

impl GetVariable {
    pub fn new_boxed(variable: String) -> Box<GetVariable> {
        Box::new(GetVariable { variable })
    }
}


// Instruction implementations

impl Instruction for LoadImmediate {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_accumulator(self.value);
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


impl Instruction for CompareEq {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i32((lhs == rhs) as i32));
    }

    fn to_string(&self) -> String { format!("CompareEq {}", self.register) }
}

impl Instruction for CompareNotEq {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i32((lhs != rhs) as i32));
    }

    fn to_string(&self) -> String { format!("CompareNotEq {}", self.register) }
}

impl Instruction for CompareGreaterThan {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i32((lhs > rhs) as i32));
    }

    fn to_string(&self) -> String { format!("CompareGreaterThan {}", self.register) }
}

impl Instruction for CompareLessThan {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i32((lhs < rhs) as i32));
    }

    fn to_string(&self) -> String { format!("CompareLessThan {}", self.register) }
}

impl Instruction for Call {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_call_arguments(Some(self.arguments.clone()));
        context.set_call(&self.block_id);
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

impl Instruction for SetVariable {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_variable(&self.variable, context.get_accumulator());
    }

    fn to_string(&self) -> String {
        format!("SetVariable {}", self.variable)
    }
}

impl Instruction for GetVariable {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_variable(&self.variable);
        context.set_accumulator(value);
    }

    fn to_string(&self) -> String {
        format!("GetVariable {}", self.variable)
    }
}
