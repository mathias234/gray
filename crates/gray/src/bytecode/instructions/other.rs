use crate::bytecode::label::Label;
use crate::interpreter::array::Array;
use crate::interpreter::iterator::{ArrayIterator, IteratorHolder};
use crate::{
    bytecode::register::Register,
    interpreter::{
        interpreter::{ExecutionContext, VariableHandle},
        value::DataValue,
        value::Value,
    },
};
use std::cell::RefCell;
use std::fmt::Write as FmtWrite;
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
        Box::new(LoadArgument { argument })
    }
}

pub struct ParamsList {
    start: usize,
}

#[allow(dead_code)]
impl ParamsList {
    pub fn new_boxed(start: usize) -> Box<ParamsList> {
        Box::new(ParamsList { start })
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
            return Box::new(Call {
                block_id,
                arguments: args.unwrap(),
            });
        }

        Box::new(Call {
            block_id,
            arguments: Vec::new(),
        })
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
        Box::new(PushBreakContinueScope {
            break_label,
            continue_label,
        })
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

pub struct Range {
    rhs_register: Register,
}

impl Range {
    pub fn new_boxed(rhs_register: Register) -> Box<Range> {
        Box::new(Range { rhs_register })
    }
}

pub struct CreateIterator {}

impl CreateIterator {
    pub fn new_boxed() -> Box<CreateIterator> {
        Box::new(CreateIterator {})
    }
}

pub struct IteratorGetNext {}

impl IteratorGetNext {
    pub fn new_boxed() -> Box<IteratorGetNext> {
        Box::new(IteratorGetNext {})
    }
}

pub struct IteratorEmpty {}

impl IteratorEmpty {
    pub fn new_boxed() -> Box<IteratorEmpty> {
        Box::new(IteratorEmpty {})
    }
}

pub struct NegateValue {}

impl NegateValue {
    pub fn new_boxed() -> Box<NegateValue> {
        Box::new(NegateValue {})
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

impl Instruction for ParamsList {
    fn execute(&self, context: &mut ExecutionContext) {
        let mut array = Array::new();

        for arg in self.start..context.get_argument_count() {
            array.push(context.get_argument(arg));
        }

        context.set_accumulator(Value::from_array(array));
    }

    fn to_string(&self) -> String {
        format!("ParamsList argument: {}", self.start)
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

    fn to_string(&self) -> String {
        format!("Return")
    }
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

impl Instruction for Range {
    fn execute(&self, context: &mut ExecutionContext) {
        let lhs_value = context.get_accumulator();
        let rhs_value = context.get_register(&self.rhs_register);

        let lhs_integer = match lhs_value.get_data_value() {
            DataValue::I64(v) => *v,
            _ => {
                context.throw_error("Expected integer");
                return;
            }
        };

        let rhs_integer = match rhs_value.get_data_value() {
            DataValue::I64(v) => *v,
            _ => {
                context.throw_error("Expected integer");
                return;
            }
        };

        if lhs_integer > rhs_integer {
            context.throw_error("Expected end to be larger then start");
            return;
        }

        context.set_accumulator(Value::from_range(lhs_integer, rhs_integer));
    }

    fn to_string(&self) -> String {
        format!("Range")
    }
}

impl Instruction for CreateIterator {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();

        let iterator = match &value.get_data_value() {
            DataValue::Range(range) => IteratorHolder {
                iterator: Rc::from(RefCell::from(range.clone())),
            },
            DataValue::Array(array) => {
                let array_iter = ArrayIterator {
                    array: array.clone(),
                    index: 0,
                };

                IteratorHolder {
                    iterator: Rc::from(RefCell::from(array_iter)),
                }
            }

            value => {
                context.throw_error(&format!("Could not convert {:?} to iterator", value));
                return;
            }
        };

        context.set_accumulator(Value::from_iterator(iterator));
    }

    fn to_string(&self) -> String {
        format!("CreateIterator")
    }
}

impl Instruction for IteratorGetNext {
    fn execute(&self, context: &mut ExecutionContext) {
        let mut iterator = context.get_accumulator();
        let iterator = &mut iterator.get_data_value_mut();
        let iterator = match iterator {
            DataValue::Iterator(iterator) => iterator,
            _ => {
                context.throw_error("Expected an iterator");
                return;
            }
        };

        if let Some(value) = iterator.iterator.borrow_mut().pop_next() {
            context.set_accumulator(value);
            return;
        }

        context.set_accumulator(Value::from_undefined());
    }

    fn to_string(&self) -> String {
        format!("IteratorGetNext")
    }
}

impl Instruction for IteratorEmpty {
    fn execute(&self, context: &mut ExecutionContext) {
        let mut iterator = context.get_accumulator();
        let iterator = &mut iterator.get_data_value_mut();
        let iterator = match iterator {
            DataValue::Iterator(iterator) => iterator,
            _ => {
                context.throw_error("Expected an iterator");
                return;
            }
        };

        if iterator.iterator.borrow_mut().empty() {
            context.set_accumulator(Value::from_i64(1));
            return;
        }

        context.set_accumulator(Value::from_i64(0));
    }

    fn to_string(&self) -> String {
        format!("IteratorEmpty")
    }
}

impl Instruction for NegateValue {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();

        let result = match value.get_data_value() {
            DataValue::I64(v) => Value::from_i64(v * -1),
            DataValue::F64(v) => Value::from_f64(v * -1.0),
            v => {
                context.throw_error(&format!("Unable to negate value {:?}", v));
                return;
            }
        };

        context.set_accumulator(result)
    }

    fn to_string(&self) -> String {
        format!("IteratorEmpty")
    }
}
