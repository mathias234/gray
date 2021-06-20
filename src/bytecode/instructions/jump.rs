use crate::bytecode::label::Label;
use crate::interpreter::interpreter::ExecutionContext;
use crate::bytecode::instructions::other::Instruction;
use crate::interpreter::value::Value;

pub struct Jump {
    target: Label,
}

#[allow(dead_code)]
impl Jump {
    pub fn new_boxed(target: Label) -> Box<Jump> {
        Box::new(Jump { target })
    }
}

pub struct JumpNotZero {
    target: Label,
}

#[allow(dead_code)]
impl JumpNotZero {
    pub fn new_boxed(target: Label) -> Box<JumpNotZero> {
        Box::new(JumpNotZero { target })
    }
}

pub struct JumpZero {
    target: Label,
}

#[allow(dead_code)]
impl JumpZero {
    pub fn new_boxed(target: Label) -> Box<JumpZero> {
        Box::new(JumpZero { target })
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
        if context.get_accumulator() != Value::from_i32(0) {
            context.set_jump_target(&self.target)
        }
    }

    fn to_string(&self) -> String { format!("JumpNotZero {}", self.target) }
}

impl Instruction for JumpZero {
    fn execute(&self, context: &mut ExecutionContext) {
        if context.get_accumulator() == Value::from_i32(0) {
            context.set_jump_target(&self.target)
        }
    }

    fn to_string(&self) -> String { format!("JumpZero {}", self.target) }
}