use crate::interpreter::value::Value;

pub type FunctionPointer = fn(Vec<Value>) -> Value;