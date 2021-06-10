use super::interpreter::Interpreter;
use super::register::Register;
use crate::bytecode::interpreter::ExecutionContext;

pub trait Instruction {
    fn execute(&self, context: &mut ExecutionContext);
    fn to_string(&self) -> String;
}

pub struct LoadImmediate {
    value: i64,
}

impl LoadImmediate {
    pub fn new_boxed(value: i64) -> Box<LoadImmediate> {
        Box::new(LoadImmediate { value })
    }
}

pub struct Store {
    register: Register,
}

impl Store {
    pub fn new_boxed(register: Register) -> Box<Store> {
        Box::new(Store { register })
    }
}


pub struct Add {
    register: Register,
}

impl Add {
    pub fn new_boxed(register: Register) -> Box<Add> {
        Box::new(Add { register })
    }
}

impl Instruction for LoadImmediate {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_accumulator(self.value);
    }

    fn to_string(&self) -> String {
        format!("LoadImmediate value: {}", self.value)
    }
}

impl Instruction for Store {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_register(&self.register, context.get_accumulator());
        context.set_accumulator(0);
    }

    fn to_string(&self) -> String {
        format!("Store {}", self.register)
    }
}

impl Instruction for Add {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();
        let value2 = context.get_register(&self.register);
        context.set_accumulator(value + value2);
    }

    fn to_string(&self) -> String {
        format!("Add {}", self.register)
    }
}