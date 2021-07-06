use crate::bytecode::register::Register;
use crate::bytecode::instructions::other::Instruction;
use crate::interpreter::interpreter::ExecutionContext;

pub struct Add {
    register: Register,
}

#[allow(dead_code)]
impl Add {
    pub fn new_boxed(register: Register) -> Box<Add> {
        Box::new(Add { register })
    }
}

pub struct Subtract {
    register: Register,
}

#[allow(dead_code)]
impl Subtract {
    pub fn new_boxed(register: Register) -> Box<Subtract> {
        Box::new(Subtract { register })
    }
}

pub struct Multiply {
    register: Register,
}

#[allow(dead_code)]
impl Multiply {
    pub fn new_boxed(register: Register) -> Box<Multiply> {
        Box::new(Multiply { register })
    }
}

pub struct Divide {
    register: Register,
}

#[allow(dead_code)]
impl Divide {
    pub fn new_boxed(register: Register) -> Box<Divide> {
        Box::new(Divide { register })
    }
}

impl Instruction for Add {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();
        let value2 = context.get_register(&self.register);
        context.set_accumulator(value.add(value2));
    }

    fn to_string(&self) -> String {
        format!("Add {}", self.register)
    }
}

impl Instruction for Subtract {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();
        let value2 = context.get_register(&self.register);
        context.set_accumulator(value.sub(value2));
    }

    fn to_string(&self) -> String {
        format!("Subtract {}", self.register)
    }
}

impl Instruction for Multiply {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();
        let value2 = context.get_register(&self.register);
        context.set_accumulator(value.mul(value2));
    }

    fn to_string(&self) -> String {
        format!("Multiply {}", self.register)
    }
}


impl Instruction for Divide {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();
        let value2 = context.get_register(&self.register);
        context.set_accumulator(value.div(value2));
    }

    fn to_string(&self) -> String {
        format!("Divide {}", self.register)
    }
}