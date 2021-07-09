use crate::interpreter::value::{Value, DataValue, Pointer};
use crate::interpreter::array::Array;
use std::any::Any;
use crate::interpreter::object::Object;
use crate::interpreter::interpreter::ExecutionContext;
use std::rc::Rc;
use std::cell::RefCell;

pub type FunctionPointer = fn(&ExecutionContext, FunctionArgs) -> Value;


pub struct FunctionArgs {
    args: Vec<Value>,
    index: usize,
}

impl FunctionArgs {
    pub fn new(args: Vec<Value>) -> FunctionArgs {
        FunctionArgs {
            args,
            index: 0,
        }
    }

    pub fn get_next(&mut self, context: &ExecutionContext) -> Value {
        if self.index >= self.len() {
            return context.throw_error(&format!("Expected at least {} arguments got only {}", self.index + 1, self.args.len()));
        }

        let result = self.args.remove(self.index);
        self.index += 1;
        return result;
    }

    pub fn get_next_i64(&mut self, context: &ExecutionContext) -> i64 {
        let value = self.get_next(context);

        match value.get_data_value() {
            DataValue::I64(v) => *v,
            v => {
                context.throw_error(&format!("Expected next value to be integer was {:?}", v));
                -1
            }
        }
    }

    pub fn get_next_f64(&mut self, context: &ExecutionContext) -> f64 {
        let value = self.get_next(context);

        match value.get_data_value() {
            DataValue::F64(v) => *v,
            v => {
                context.throw_error(&format!("Expected next value to be float was {:?}", v));
                0.0
            }
        }
    }

    pub fn get_next_string(&mut self, context: &ExecutionContext) -> String {
        let value = self.get_next(context);

        match value.get_data_value() {
            DataValue::String(v) => v.to_string(),
            v => {
                context.throw_error(&format!("Expected next value to be string was {:?}", v));
                String::new()
            }
        }
    }

    pub fn get_next_pointer(&mut self, context: &ExecutionContext) -> Pointer<dyn Any> {
        let value = self.get_next(context);

        match value.get_data_value() {
            DataValue::Pointer(v) => v.clone(),
            v => {
                context.throw_error(&format!("Expected next value to be pointer was {:?}", v));
                Rc::new(RefCell::new(-1))
            }
        }
    }

    pub fn get_next_object(&mut self, context: &ExecutionContext) -> Object {
        let value = self.get_next(context);

        match value.get_data_value() {
            DataValue::Object(v) => v.clone(),
            v => {
                context.throw_error(&format!("Expected next value to be object was {:?}", v));
                Object::new()
            }
        }
    }

    pub fn get_next_array(&mut self, context: &ExecutionContext) -> Array {
        let value = self.get_next(context);

        match value.get_data_value() {
            DataValue::Array(v) => v.clone(),
            v => {
                context.throw_error(&format!("Expected next value to be array was {:?}", v));
                Array::new()
            }
        }
    }

    pub fn len(&self) -> usize {
        self.args.len()
    }
}