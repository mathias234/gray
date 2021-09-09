use crate::interpreter::array::Array;
use crate::interpreter::interpreter::ExecutionContext;
use crate::interpreter::iterator::{IteratorHolder, RangeIterator};
use crate::interpreter::object::Object;
use std::any::Any;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

use super::struct_def::StructDef;

pub type Pointer<T> = Rc<RefCell<T>>;

#[derive(Clone, Debug)]
pub enum DataValue {
    I64(i64),
    F64(f64),
    Object(Object),
    String(Rc<String>),
    Array(Array),
    Pointer(Pointer<dyn Any>),
    FunctionPointer(Rc<String>),
    Range(RangeIterator),
    Iterator(IteratorHolder),
    StructDef(StructDef),
    Undefined,
}

#[derive(Clone, Debug)]
pub struct Value {
    value: DataValue,
}

impl Value {
    pub fn from_i64(value: i64) -> Value {
        Value {
            value: DataValue::I64(value),
        }
    }

    pub fn from_f64(value: f64) -> Value {
        Value {
            value: DataValue::F64(value),
        }
    }

    pub fn from_object(value: Object) -> Value {
        Value {
            value: DataValue::Object(value),
        }
    }

    pub fn from_string(value: Rc<String>) -> Value {
        Value {
            value: DataValue::String(value),
        }
    }

    pub fn from_array(value: Array) -> Value {
        Value {
            value: DataValue::Array(value),
        }
    }

    pub fn from_any<T: Any>(value: T) -> Value {
        Value {
            value: DataValue::Pointer(Rc::from(RefCell::from(value))),
        }
    }

    pub fn from_undefined() -> Value {
        Value {
            value: DataValue::Undefined,
        }
    }

    pub fn from_function(name: Rc<String>) -> Value {
        Value {
            value: DataValue::FunctionPointer(name),
        }
    }

    pub fn from_range(from: i64, to: i64) -> Value {
        Value {
            value: DataValue::Range(RangeIterator {
                from,
                to,
                index: from,
            }),
        }
    }

    pub fn from_iterator(iterator: IteratorHolder) -> Value {
        Value {
            value: DataValue::Iterator(iterator),
        }
    }

    pub fn from_struct_def(struct_def: StructDef) -> Value {
        Value {
            value: DataValue::StructDef(struct_def),
        }
    }

    pub fn is_undefined(&self) -> bool {
        match self.get_data_value() {
            DataValue::Undefined => true,
            _ => false,
        }
    }

    pub fn as_struct_def(&self) -> Option<StructDef> {
        match self.get_data_value() {
            DataValue::StructDef(def) => Some(def.clone()),
            _ => None,
        }
    }

    pub fn make_undefined(&mut self) {
        self.value = DataValue::Undefined;
    }

    pub fn get_data_value(&self) -> &DataValue {
        &self.value
    }

    pub fn get_data_value_mut(&mut self) -> &mut DataValue {
        &mut self.value
    }

    pub fn add(self, context: &mut ExecutionContext, rhs_value: Value) -> Value {
        match &self.value {
            DataValue::I64(lhs) => match &rhs_value.value {
                DataValue::I64(rhs) => Value::from_i64(*lhs + *rhs),
                DataValue::F64(rhs) => Value::from_f64(*lhs as f64 + *rhs),
                rhs => context.throw_error(&format!("Cannot convert {:?} to integer", rhs)),
            },
            DataValue::F64(lhs) => match &rhs_value.value {
                DataValue::I64(rhs) => Value::from_f64(lhs + *rhs as f64),
                DataValue::F64(rhs) => Value::from_f64(lhs + rhs),
                rhs => context.throw_error(&format!("Cannot convert {:?} to float", rhs)),
            },
            DataValue::String(lhs) => match &rhs_value.value {
                DataValue::String(rhs) => Value::from_string(Rc::from(format!("{}{}", lhs, rhs))),
                rhs => context.throw_error(&format!("Cannot convert {:?} to string", rhs)),
            },
            rhs => {
                context.throw_error(&format!("Add operator is not implemented for {:?}", rhs));
                Value::from_i64(-1)
            }
        }
    }

    pub fn sub(self, context: &mut ExecutionContext, rhs_value: Value) -> Value {
        match self.value {
            DataValue::I64(lhs) => match rhs_value.value {
                DataValue::I64(rhs) => Value::from_i64(lhs - rhs),
                DataValue::F64(rhs) => Value::from_f64(lhs as f64 - rhs),
                rhs => context.throw_error(&format!("Cannot convert {:?} to integer", rhs)),
            },
            DataValue::F64(lhs) => match rhs_value.value {
                DataValue::I64(rhs) => Value::from_f64(lhs - rhs as f64),
                DataValue::F64(rhs) => Value::from_f64(lhs - rhs),
                rhs => context.throw_error(&format!("Cannot convert {:?} to float", rhs)),
            },
            rhs => {
                context.throw_error(&format!(
                    "Subtract operator is not implemented for {:?}",
                    rhs
                ));
                Value::from_i64(-1)
            }
        }
    }

    pub fn mul(self, context: &mut ExecutionContext, rhs_value: Value) -> Value {
        match self.value {
            DataValue::I64(lhs) => match rhs_value.value {
                DataValue::I64(rhs) => Value::from_i64(lhs * rhs),
                DataValue::F64(rhs) => Value::from_f64(lhs as f64 * rhs),
                rhs => context.throw_error(&format!("Cannot convert {:?} to integer", rhs)),
            },
            DataValue::F64(lhs) => match rhs_value.value {
                DataValue::I64(rhs) => Value::from_f64(lhs * rhs as f64),
                DataValue::F64(rhs) => Value::from_f64(lhs * rhs),
                rhs => context.throw_error(&format!("Cannot convert {:?} to float", rhs)),
            },
            rhs => {
                context.throw_error(&format!(
                    "Multiply operator is not implemented for {:?}",
                    rhs
                ));
                Value::from_i64(-1)
            }
        }
    }

    pub fn div(self, context: &mut ExecutionContext, rhs_value: Value) -> Value {
        match self.value {
            DataValue::I64(lhs) => match rhs_value.value {
                DataValue::I64(rhs) => Value::from_i64(lhs / rhs),
                DataValue::F64(rhs) => Value::from_f64(lhs as f64 / rhs),
                rhs => context.throw_error(&format!("Cannot convert {:?} to integer", rhs)),
            },
            DataValue::F64(lhs) => match rhs_value.value {
                DataValue::I64(rhs) => Value::from_f64(lhs / rhs as f64),
                DataValue::F64(rhs) => Value::from_f64(lhs / rhs),
                rhs => context.throw_error(&format!("Cannot convert {:?} to float", rhs)),
            },
            rhs => {
                context.throw_error(&format!("Divide operator is not implemented for {:?}", rhs));
                Value::from_i64(-1)
            }
        }
    }

    pub fn eq(&self, rhs_value: &Self) -> bool {
        match &self.value {
            DataValue::I64(lhs) => match &rhs_value.value {
                DataValue::I64(rhs) => *lhs == *rhs,
                DataValue::F64(rhs) => *lhs as f64 == *rhs,
                _ => false,
            },
            DataValue::F64(lhs) => match &rhs_value.value {
                DataValue::I64(rhs) => *lhs == *rhs as f64,
                DataValue::F64(rhs) => *lhs == *rhs,
                _ => false,
            },
            DataValue::String(lhs) => match &rhs_value.value {
                DataValue::String(rhs) => lhs == rhs,
                _ => false,
            },
            _ => false,
        }
    }

    pub fn partial_cmp(&self, context: &mut ExecutionContext, rhs_value: &Self) -> Option<Ordering> {
        match self.value.clone() {
            DataValue::I64(lhs) => match &rhs_value.value {
                DataValue::I64(rhs) => lhs.partial_cmp(&rhs),
                DataValue::F64(rhs) => (lhs as f64).partial_cmp(&rhs),
                rhs => {
                    context.throw_error(&format!("Unable to compare {:?} to {:?}", lhs, rhs));
                    None
                }
            },
            DataValue::F64(lhs) => match &rhs_value.value {
                DataValue::I64(rhs) => lhs.partial_cmp(&(*rhs as f64)),
                DataValue::F64(rhs) => lhs.partial_cmp(&rhs),
                rhs => {
                    context.throw_error(&format!("Unable to compare {:?} to {:?}", lhs, rhs));
                    None
                }
            },
            rhs => {
                context.throw_error(&format!(
                    "Compare operator is not implemented for {:?}",
                    rhs
                ));
                None
            }
        }
    }

    pub fn to_string(&self) -> String {
        match self.get_data_value() {
            DataValue::F64(float_value) => format!("{}", float_value),
            DataValue::I64(int_value) => format!("{}", int_value),
            DataValue::Object(object) => format!("{:?}", object),
            DataValue::String(string) => format!("{}", string),
            DataValue::Array(array) => format!("{}", array),
            DataValue::FunctionPointer(_) => format!("FunctionPointer"),
            DataValue::Range(range) => format!("{}..{}", range.from, range.to),
            DataValue::Pointer(_) => format!("Internal Pointer"),
            DataValue::Iterator(_) => format!("Iterator"),
            DataValue::Undefined => format!("Undefined"),
            DataValue::StructDef(_) => format!("StructDef"),
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
            DataValue::FunctionPointer(v) => write!(f, "({}: FunctionPointer)", v.clone()),
            DataValue::Range(range) => write!(f, "{}..{}", range.from, range.to),
            DataValue::Pointer(_) => write!(f, "(Internal Pointer)"),
            DataValue::Iterator(_) => write!(f, "Iterator"),
            DataValue::Undefined => write!(f, "(Undefined)"),
            DataValue::StructDef(_) => write!(f, "StructDef"),
        }
    }
}
