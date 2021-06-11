use super::interpreter::Interpreter;
use super::register::Register;
use crate::bytecode::interpreter::ExecutionContext;
use crate::bytecode::label::Label;

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

pub struct LoadRegister {
    register: Register,
}

impl LoadRegister {
    pub fn new_boxed(register: Register) -> Box<LoadRegister> {
        Box::new(LoadRegister { register })
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

pub struct Subtract {
    register: Register,
}

impl Subtract {
    pub fn new_boxed(register: Register) -> Box<Subtract> {
        Box::new(Subtract { register })
    }
}

pub struct Jump {
    target: Label,
}

impl Jump {
    pub fn new_boxed(target: Label) -> Box<Jump> {
        Box::new(Jump { target })
    }
}

pub struct JumpNotZero {
    target: Label,
}

impl JumpNotZero {
    pub fn new_boxed(target: Label) -> Box<JumpNotZero> {
        Box::new(JumpNotZero { target })
    }
}

pub struct JumpZero {
    target: Label,
}

impl JumpZero {
    pub fn new_boxed(target: Label) -> Box<JumpZero> {
        Box::new(JumpZero { target })
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

impl Instruction for Subtract {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();
        let value2 = context.get_register(&self.register);
        context.set_accumulator(value - value2);
    }

    fn to_string(&self) -> String {
        format!("Subtract {}", self.register)
    }
}

impl Instruction for Jump {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_jump_target(&self.target)
    }

    fn to_string(&self) -> String { format!("Jump {}", self.target) }
}

impl Instruction for JumpNotZero {
    fn execute(&self, context: &mut ExecutionContext) {
        if context.get_accumulator() != 0 {
            context.set_jump_target(&self.target)
        }
    }

    fn to_string(&self) -> String { format!("JumpNotZero {}", self.target) }
}

impl Instruction for JumpZero {
    fn execute(&self, context: &mut ExecutionContext) {
        if context.get_accumulator() == 0 {
            context.set_jump_target(&self.target)
        }
    }

    fn to_string(&self) -> String { format!("JumpZero {}", self.target) }
}
