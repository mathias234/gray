use crate::bytecode::instructions::other::Instruction;
use crate::bytecode::register::Register;
use crate::interpreter::interpreter::ExecutionContext;
use crate::interpreter::value::Value;
use std::cmp::Ordering;

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

pub struct CompareLessThanOrEqual {
    register: Register,
}

#[allow(dead_code)]
impl CompareLessThanOrEqual {
    pub fn new_boxed(register: Register) -> Box<CompareLessThanOrEqual> {
        Box::new(CompareLessThanOrEqual { register })
    }
}


pub struct CompareGreaterThanOrEqual {
    register: Register,
}

#[allow(dead_code)]
impl CompareGreaterThanOrEqual {
    pub fn new_boxed(register: Register) -> Box<CompareGreaterThanOrEqual> {
        Box::new(CompareGreaterThanOrEqual { register })
    }
}

pub struct And {
    register: Register,
}

#[allow(dead_code)]
impl And {
    pub fn new_boxed(register: Register) -> Box<And> {
        Box::new(And { register })
    }
}

pub struct Or {
    register: Register,
}

#[allow(dead_code)]
impl Or {
    pub fn new_boxed(register: Register) -> Box<Or> {
        Box::new(Or { register })
    }
}

impl Instruction for CompareEq {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i64((lhs.eq(&rhs)) as i64));
    }

    fn to_string(&self) -> String { format!("CompareEq {}", self.register) }
}

impl Instruction for CompareNotEq {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i64((!lhs.eq(&rhs)) as i64));
    }

    fn to_string(&self) -> String { format!("CompareNotEq {}", self.register) }
}

impl Instruction for CompareGreaterThan {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i64((lhs.partial_cmp(context, &rhs).unwrap() == Ordering::Greater) as i64));
    }

    fn to_string(&self) -> String { format!("CompareGreaterThan {}", self.register) }
}

impl Instruction for CompareLessThan {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i64((lhs.partial_cmp(context, &rhs).unwrap() == Ordering::Less) as i64));
    }

    fn to_string(&self) -> String { format!("CompareLessThan {}", self.register) }
}

impl Instruction for CompareLessThanOrEqual {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i64((lhs.partial_cmp(context, &rhs).unwrap() != Ordering::Greater) as i64));
    }

    fn to_string(&self) -> String { format!("CompareLessThanOrEqual {}", self.register) }
}

impl Instruction for CompareGreaterThanOrEqual {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        context.set_accumulator(Value::from_i64((lhs.partial_cmp(context, &rhs).unwrap() != Ordering::Less) as i64));
    }

    fn to_string(&self) -> String { format!("CompareGreaterThanOrEqual {}", self.register) }
}

impl Instruction for And {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        let true_value = Value::from_i64(1);
        if lhs.eq(&true_value) && rhs.eq(&true_value) {
            context.set_accumulator(true_value);
            return;
        }

        context.set_accumulator(Value::from_i64(0));
    }

    fn to_string(&self) -> String { format!("And {}", self.register) }
}

impl Instruction for Or {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs = context.get_accumulator();
        let rhs = context.get_register(&self.register);

        let true_value = Value::from_i64(1);
        if lhs.eq(&true_value) || rhs.eq(&true_value) {
            context.set_accumulator(true_value);
            return;
        }

        context.set_accumulator(Value::from_i64(0));
    }

    fn to_string(&self) -> String { format!("Or {}", self.register) }
}