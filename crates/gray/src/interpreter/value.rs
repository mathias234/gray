use std::{fmt};
use std::cmp::Ordering;
use crate::interpreter::object::Object;
use crate::interpreter::array::Array;
use std::rc::Rc;
use std::any::Any;
use std::cell::RefCell;
use crate::interpreter::interpreter::ExecutionContext;

pub type Pointer<T> = Rc<RefCell<T>>;

#[derive(Clone, Debug)]
pub enum DataValue {
    I64(i64),
    F64(f64),
    Object(Object),
    String(Rc<String>),
    Array(Array),
    Pointer(Pointer<dyn Any>),
    Undefined,
}

#[derive(Clone, Debug)]
pub struct Value {
    value: DataValue,
}

impl Value {
    pub fn from_i64(value: i64) -> Value {
        Value {
            value: DataValue::I64(value)
        }
    }

    pub fn from_f64(value: f64) -> Value {
        Value {
            value: DataValue::F64(value)
        }
    }

    pub fn from_object(value: Object) -> Value {
        Value {
            value: DataValue::Object(value)
        }
    }

    pub fn from_string(value: Rc<String>) -> Value {
        Value {
            value: DataValue::String(value)
        }
    }

    pub fn from_array(value: Array) -> Value {
        Value {
            value: DataValue::Array(value)
        }
    }

    pub fn from_any<T: Any>(value: T) -> Value {
        Value {
            value: DataValue::Pointer(Rc::from(RefCell::from(value))),
        }
    }

    pub fn from_undefined() -> Value {
        Value {
            value: DataValue::Undefined
        }
    }

    pub fn is_undefined(&self) -> bool {
        match self.get_data_value() {
            DataValue::Undefined => true,
            _ => false,
        }
    }

    pub fn get_data_value(&self) -> &DataValue {
        &self.value
    }

    pub fn get_data_value_mut(&mut self) -> &mut DataValue {
        &mut self.value
    }

    pub fn add(self, context: &ExecutionContext, rhs_value: Value) -> Value {
        match &self.value {
            DataValue::I64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_i64(*lhs + *rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(*lhs as f64 + *rhs) }
                    rhs => context.throw_error(&format!("Cannot convert {:?} to integer", rhs)),
                }
            }
            DataValue::F64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_f64(lhs + *rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs + rhs) }
                    rhs => context.throw_error(&format!("Cannot convert {:?} to float", rhs)),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Add operator is not implemented for objects"),
                }
            }
            DataValue::String(lhs) => {
                match &rhs_value.value {
                    DataValue::String(rhs) => {
                        Value::from_string(Rc::from(format!("{}{}", lhs, rhs)))
                    }
                    rhs => context.throw_error(&format!("Cannot convert {:?} to string", rhs)),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Add operator is not implemented for arrays"),
                }
            }
            DataValue::Pointer(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Add operator is not implemented for pointers"),
                }
            }
            DataValue::Undefined => {
                match &rhs_value.value {
                    _ => context.throw_error("Add operator is not implemented for undefined"),
                }
            }
        }
    }

    pub fn sub(self, context: &ExecutionContext, rhs_value: Value) -> Value {
        match self.value {
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_i64(lhs - rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 - rhs) }
                    rhs => context.throw_error(&format!("Cannot convert {:?} to integer", rhs)),
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_f64(lhs - rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs - rhs) }
                    rhs => context.throw_error(&format!("Cannot convert {:?} to float", rhs)),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Subtract operator is not implemented for objects"),
                }
            }
            DataValue::String(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Subtract operator is not implemented for strings"),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Subtract operator is not implemented for arrays"),
                }
            }
            DataValue::Pointer(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Subtract operator is not implemented for pointers"),
                }
            }
            DataValue::Undefined => {
                match &rhs_value.value {
                    _ => context.throw_error("Subtract operator is not implemented for undefined"),
                }
            }
        }
    }

    pub fn mul(self, context: &ExecutionContext, rhs_value: Value) -> Value {
        match self.value {
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_i64(lhs * rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 * rhs) }
                    rhs => context.throw_error(&format!("Cannot convert {:?} to integer", rhs)),
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_f64(lhs * rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs * rhs) }
                    rhs => context.throw_error(&format!("Cannot convert {:?} to float", rhs)),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Multiply operator is not implemented for object"),
                }
            }
            DataValue::String(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Multiply operator is not implemented for strings"),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Multiply operator is not implemented for arrays"),
                }
            }
            DataValue::Pointer(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Multiply operator is not implemented for pointers"),
                }
            }
            DataValue::Undefined => {
                match &rhs_value.value {
                    _ => context.throw_error("Multiply operator is not implemented for undefined"),
                }
            }
        }
    }

    pub fn div(self, context: &ExecutionContext, rhs_value: Value) -> Value {
        match self.value {
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_i64(lhs / rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 / rhs) }
                    rhs => context.throw_error(&format!("Cannot convert {:?} to integer", rhs)),
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_f64(lhs / rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs / rhs) }
                    rhs => context.throw_error(&format!("Cannot convert {:?} to float", rhs)),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Divide operator is not implemented for objects"),
                }
            }
            DataValue::String(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Divide operator is not implemented for strings"),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Divide operator is not implemented for arrays"),
                }
            }
            DataValue::Pointer(_) => {
                match &rhs_value.value {
                    _ => context.throw_error("Divide operator is not implemented for pointers"),
                }
            }
            DataValue::Undefined => {
                match &rhs_value.value {
                    _ => context.throw_error("Divide operator is not implemented for undefined"),
                }
            }
        }
    }

    pub fn eq(&self, context: &ExecutionContext, rhs_value: &Self) -> bool {
        match &self.value {
            DataValue::I64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { *lhs == *rhs }
                    DataValue::F64(rhs) => { *lhs as f64 == *rhs }
                    rhs => {
                        context.throw_error(&format!("Unable to compare {:?} to {:?}", lhs, rhs));
                        false
                    }
                }
            }
            DataValue::F64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { *lhs == *rhs as f64 }
                    DataValue::F64(rhs) => { *lhs == *rhs }
                    rhs => {
                        context.throw_error(&format!("Unable to compare {:?} to {:?}", lhs, rhs));
                        false
                    }
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => {
                        context.throw_error("Equal operator is not implemented for objects");
                        false
                    }
                }
            }
            DataValue::String(lhs) => {
                match &rhs_value.value {
                    DataValue::String(rhs) => { lhs == rhs }
                    _ => {
                        context.throw_error("Cannot compare a string to a basic value");
                        false
                    }
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => {
                        context.throw_error("Equal operator is not implemented for arrays");
                        false
                    }
                }
            }
            DataValue::Pointer(_) => {
                match &rhs_value.value {
                    _ => {
                        context.throw_error("Equal operator is not implemented for pointers");
                        false
                    }
                }
            }
            DataValue::Undefined => {
                match &rhs_value.value {
                    _ => {
                        context.throw_error("Equal operator is not implemented for undefined");
                        false
                    }
                }
            }
        }
    }

    pub fn partial_cmp(&self, context: &ExecutionContext, rhs_value: &Self) -> Option<Ordering> {
        match self.value.clone() {
            DataValue::I64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { lhs.partial_cmp(&rhs) }
                    DataValue::F64(rhs) => { (lhs as f64).partial_cmp(&rhs) }
                    rhs => {
                        context.throw_error(&format!("Unable to compare {:?} to {:?}", lhs, rhs));
                        None
                    }
                }
            }
            DataValue::F64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { lhs.partial_cmp(&(*rhs as f64)) }
                    DataValue::F64(rhs) => { lhs.partial_cmp(&rhs) }
                    rhs => {
                        context.throw_error(&format!("Unable to compare {:?} to {:?}", lhs, rhs));
                        None
                    }
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => {
                        context.throw_error("Compare operator is not implemented for objects");
                        None
                    }
                }
            }
            DataValue::String(lhs) => {
                match &rhs_value.value {
                    DataValue::String(rhs) => { lhs.partial_cmp(&rhs) }
                    _ => {
                        context.throw_error("Cannot compare a string to with a basic value");
                        None
                    }
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => {
                        context.throw_error("Compare operator is not implemented for arrays");
                        None
                    }
                }
            }
            DataValue::Pointer(_) => {
                match &rhs_value.value {
                    _ => {
                        context.throw_error("Compare operator is not implemented for pointers");
                        None
                    }
                }
            }
            DataValue::Undefined => {
                match &rhs_value.value {
                    _ => {
                        context.throw_error("Compare operator is not implemented for undefined");
                        None
                    }
                }
            }
        }
    }
}


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.value.clone() {
            DataValue::I64(v) => write!(f, "({}: i64)", v),
            DataValue::F64(v) => write!(f, "({}: f64)", v),
            DataValue::Object(v) => write!(f, "({:?}: Object)", v.clone()),
            DataValue::String(v) => write!(f, "({}: String)", v.clone()),
            DataValue::Array(v) => write!(f, "({}: Array)", v.clone()),
            DataValue::Pointer(_) => write!(f, "(Internal Pointer)"),
            DataValue::Undefined => write!(f, "(Undefined)"),
        }
    }
}
