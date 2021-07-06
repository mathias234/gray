use crate::interpreter::value::{Value, DataValue, Pointer};
use crate::interpreter::array::Array;
use std::any::Any;
use crate::interpreter::object::Object;
use crate::interpreter::interpreter::ExecutionContext;

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

    pub fn get_next(&mut self) -> &Value {
        if self.index >= self.len() {
            panic!("Expected at least {} arguments got only {}", self.index + 1, self.args.len())
        }

        let result = &self.args[self.index];
        self.index += 1;
        return result;
    }

    pub fn get_next_i64(&mut self) -> i64 {
        let value = self.get_next();

        match value.get_data_value() {
            DataValue::I64(v) => *v,
            v => panic!("Expected next value to be integer was {:?}", v)
        }
    }

    pub fn get_next_f64(&mut self) -> f64 {
        let value = self.get_next();

        match value.get_data_value() {
            DataValue::F64(v) => *v,
            v => panic!("Expected next value to be float was {:?}", v)
        }
    }

    pub fn get_next_string(&mut self) -> String {
        let value = self.get_next();

        match value.get_data_value() {
            DataValue::String(v) => v.to_string(),
            v => panic!("Expected next value to be string was {:?}", v)
        }
    }

    pub fn get_next_pointer(&mut self) -> Pointer<dyn Any> {
        let value = self.get_next();

        match value.get_data_value() {
            DataValue::Pointer(v) => v.clone(),
            v => panic!("Expected next value to be pointer was {:?}", v)
        }
    }

    pub fn get_next_object(&mut self) -> Object {
        let value = self.get_next();

        match value.get_data_value() {
            DataValue::Object(v) => v.clone(),
            v => panic!("Expected next value to be object was {:?}", v)
        }
    }

    pub fn get_next_array(&mut self) -> Array {
        let value = self.get_next();

        match value.get_data_value() {
            DataValue::Array(v) => v.clone(),
            v => panic!("Expected next value to be array was {:?}", v)
        }
    }

    pub fn len(&self) -> usize {
        self.args.len()
    }
}