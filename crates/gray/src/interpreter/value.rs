use std::{fmt, ops, cmp};
use std::cmp::Ordering;
use crate::interpreter::object::Object;
use crate::interpreter::array::Array;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum DataValue {
    I64(i64),
    F64(f64),
    Object(Object),
    String(Rc<String>),
    Array(Array),
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

    pub fn get_data_value(&self) -> &DataValue {
        &self.value
    }

    pub fn get_data_value_mut(&mut self) -> &mut DataValue {
        &mut self.value
    }
}

// FIXME: find a better way to do this, very verbose and error prone..

impl ops::Add<Value> for Value {
    type Output = Value;

    fn add(self, rhs_value: Value) -> Self::Output {
        match &self.value {
            DataValue::I64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_i64(*lhs + *rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(*lhs as f64 + *rhs) }
                    rhs => panic!("Unable to add {:?} to {:?}", lhs, rhs),
                }
            }
            DataValue::F64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_f64(lhs + *rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs + rhs) }
                    rhs => panic!("Unable to add {:?} to {:?}", lhs, rhs),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on objects"),
                }
            }
            DataValue::String(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on strings"),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on arrays"),
                }
            }
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = Value;

    fn sub(self, rhs_value: Value) -> Self::Output {
        match self.value {
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_i64(lhs - rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 - rhs) }
                    rhs => panic!("Unable to subtract {:?} from {:?}", lhs, rhs),
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_f64(lhs - rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs - rhs) }
                    rhs => panic!("Unable to subtract {:?} from {:?}", lhs, rhs),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on objects"),
                }
            }
            DataValue::String(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on strings"),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on arrays"),
                }
            }
        }
    }
}

impl ops::Mul<Value> for Value {
    type Output = Value;

    fn mul(self, rhs_value: Value) -> Self::Output {
        match self.value {
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_i64(lhs * rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 * rhs) }
                    rhs => panic!("Unable to multiply {:?} with {:?}", lhs, rhs),
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_f64(lhs * rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs * rhs) }
                    rhs => panic!("Unable to multiply {:?} with {:?}", lhs, rhs),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on objects"),
                }
            }
            DataValue::String(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on strings"),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on arrays"),
                }
            }
        }
    }
}

impl ops::Div<Value> for Value {
    type Output = Value;

    fn div(self, rhs_value: Value) -> Self::Output {
        match self.value {
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_i64(lhs / rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 / rhs) }
                    rhs => panic!("Unable to divide {:?} by {:?}", lhs, rhs),
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I64(rhs) => { Value::from_f64(lhs / rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs / rhs) }
                    rhs => panic!("Unable to divide {:?} by {:?}", lhs, rhs),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on objects"),
                }
            }
            DataValue::String(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on strings"),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on arrays"),
                }
            }
        }
    }
}

impl cmp::PartialEq for Value {
    fn eq(&self, rhs_value: &Self) -> bool {
        match &self.value {
            DataValue::I64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { *lhs == *rhs }
                    DataValue::F64(rhs) => { *lhs as f64 == *rhs }
                    rhs => panic!("Unable to compare {:?} to {:?}", lhs, rhs),
                }
            }
            DataValue::F64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { *lhs == *rhs as f64 }
                    DataValue::F64(rhs) => { *lhs == *rhs }
                    rhs => panic!("Unable to compare {:?} to {:?}", lhs, rhs),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on objects"),
                }
            }
            DataValue::String(lhs) => {
                match &rhs_value.value {
                    DataValue::String(rhs) => { *lhs == *rhs }
                    _ => panic!("Cannot compare a string to a basic value"),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on arrays"),
                }
            }
        }
    }
}

impl cmp::PartialOrd for Value {
    fn partial_cmp(&self, rhs_value: &Self) -> Option<Ordering> {
        match self.value.clone() {
            DataValue::I64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { lhs.partial_cmp(&rhs) }
                    DataValue::F64(rhs) => { (lhs as f64).partial_cmp(&rhs) }
                    rhs => panic!("Unable to compare {:?} to {:?}", lhs, rhs),
                }
            }
            DataValue::F64(lhs) => {
                match &rhs_value.value {
                    DataValue::I64(rhs) => { lhs.partial_cmp(&(*rhs as f64)) }
                    DataValue::F64(rhs) => { lhs.partial_cmp(&rhs) }
                    rhs => panic!("Unable to compare {:?} to {:?}", lhs, rhs),
                }
            }
            DataValue::Object(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on objects"),
                }
            }
            DataValue::String(lhs) => {
                match &rhs_value.value {
                    DataValue::String(rhs) => { lhs.partial_cmp(&rhs) }
                    _ => panic!("Cannot PartialOrd a string to with a basic value"),
                }
            }
            DataValue::Array(_) => {
                match &rhs_value.value {
                    _ => panic!("Operations are currently not supported on arrays"),
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
        }
    }
}
