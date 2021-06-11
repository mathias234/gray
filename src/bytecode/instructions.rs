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

#[allow(dead_code)]
impl LoadImmediate {
    pub fn new_boxed(value: i64) -> Box<LoadImmediate> {
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

pub struct Store {
    register: Register,
}

#[allow(dead_code)]
impl Store {
    pub fn new_boxed(register: Register) -> Box<Store> {
        Box::new(Store { register })
    }
}

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

pub struct Jump {
    target: Option<Label>,
}

#[allow(dead_code)]
impl Jump {
    pub fn new_boxed(target: Option<Label>) -> Box<Jump> {
        Box::new(Jump { target })
    }
    pub fn set_target(&mut self, target: Label) { self.target = Some(target) }
}

pub struct JumpNotZero {
    target: Option<Label>,
}

#[allow(dead_code)]
impl JumpNotZero {
    pub fn new_boxed(target: Option<Label>) -> Box<JumpNotZero> {
        Box::new(JumpNotZero { target })
    }
    pub fn set_target(&mut self, target: Label) { self.target = Some(target) }
}

pub struct JumpZero {
    target: Option<Label>,
}

#[allow(dead_code)]
impl JumpZero {
    pub fn new_boxed(target: Option<Label>) -> Box<JumpZero> {
        Box::new(JumpZero { target })
    }
    pub fn set_target(&mut self, target: Label) { self.target = Some(target) }
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
    block_id: usize, // FIXME: probably should not directly use usize?
}

impl Call {
    pub fn new_boxed(block_id: usize) -> Box<Call> {
        Box::new(Call { block_id })
    }
}

pub struct Return {}

impl Return {
    pub fn new_boxed() -> Box<Return> {
        Box::new(Return {})
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
        context.set_jump_target(&self.target.unwrap())
    }

    fn to_string(&self) -> String { format!("Jump {}", self.target.unwrap()) }
}

impl Instruction for JumpNotZero {
    fn execute(&self, context: &mut ExecutionContext) {
        if context.get_accumulator() != 0 {
            context.set_jump_target(&self.target.unwrap())
        }
    }

    fn to_string(&self) -> String { format!("JumpNotZero {}", self.target.unwrap()) }
}

impl Instruction for JumpZero {
    fn execute(&self, context: &mut ExecutionContext) {
        if context.get_accumulator() == 0 {
            context.set_jump_target(&self.target.unwrap())
        }
    }

    fn to_string(&self) -> String { format!("JumpZero {}", self.target.unwrap()) }
}

impl Instruction for CompareEq {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator((lhs == rhs) as i64);
    }

    fn to_string(&self) -> String { format!("CompareEq {}", self.register) }
}

impl Instruction for CompareNotEq {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator((lhs != rhs) as i64);
    }

    fn to_string(&self) -> String { format!("CompareNotEq {}", self.register) }
}

impl Instruction for CompareGreaterThan {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator((lhs > rhs) as i64);
    }

    fn to_string(&self) -> String { format!("CompareGreaterThan {}", self.register) }
}

impl Instruction for CompareLessThan {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator((lhs < rhs) as i64);
    }

    fn to_string(&self) -> String { format!("CompareLessThan {}", self.register) }
}

impl Instruction for Call {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_call(self.block_id);
    }

    fn to_string(&self) -> String { format!("Call {}", self.block_id) }
}

impl Instruction for Return {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_return();
    }

    fn to_string(&self) -> String { format!("Return") }
}