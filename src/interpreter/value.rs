use std::{fmt, ops, cmp};
use std::cmp::Ordering;

#[derive(Copy, Clone)]
pub enum DataValue {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

#[derive(Copy, Clone)]
pub struct Value {
    value: DataValue,
}

impl Value {
    pub fn from_i32(value: i32) -> Value {
        Value {
            value: DataValue::I32(value)
        }
    }
    pub fn from_i64(value: i64) -> Value {
        Value {
            value: DataValue::I64(value)
        }
    }
    pub fn from_f32(value: f32) -> Value {
        Value {
            value: DataValue::F32(value)
        }
    }
    pub fn from_f64(value: f64) -> Value {
        Value {
            value: DataValue::F64(value)
        }
    }
}

// FIXME: find a better way to do this, very verbose and error prone..

impl ops::Add<Value> for Value {
    type Output = Value;

    fn add(self, rhs_value: Value) -> Self::Output {
        match self.value {
            DataValue::I32(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { Value::from_i32(lhs + rhs) }
                    DataValue::I64(rhs) => { Value::from_i64(lhs as i64 + rhs) }
                    DataValue::F32(rhs) => { Value::from_f32(lhs as f32 + rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 + rhs) }
                }
            }
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { Value::from_i64(lhs + rhs as i64) }
                    DataValue::I64(rhs) => { Value::from_i64(lhs + rhs) }
                    DataValue::F32(rhs) => { Value::from_f32(lhs as f32 + rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 + rhs) }
                }
            }
            DataValue::F32(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { Value::from_f32(lhs + rhs as f32) }
                    DataValue::I64(rhs) => { Value::from_f64(lhs as f64 + rhs as f64) }
                    DataValue::F32(rhs) => { Value::from_f32(lhs as f32 + rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 + rhs) }
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { Value::from_f64(lhs + rhs as f64) }
                    DataValue::I64(rhs) => { Value::from_f64(lhs + rhs as f64) }
                    DataValue::F32(rhs) => { Value::from_f64(lhs + rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs + rhs) }
                }
            }
        }
    }
}

impl ops::Sub<Value> for Value {
    type Output = Value;

    fn sub(self, rhs_value: Value) -> Self::Output {
        match self.value {
            DataValue::I32(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { Value::from_i32(lhs - rhs) }
                    DataValue::I64(rhs) => { Value::from_i64(lhs as i64 - rhs) }
                    DataValue::F32(rhs) => { Value::from_f32(lhs as f32 - rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 - rhs) }
                }
            }
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { Value::from_i64(lhs - rhs as i64) }
                    DataValue::I64(rhs) => { Value::from_i64(lhs - rhs) }
                    DataValue::F32(rhs) => { Value::from_f32(lhs as f32 - rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 - rhs) }
                }
            }
            DataValue::F32(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { Value::from_f32(lhs - rhs as f32) }
                    DataValue::I64(rhs) => { Value::from_f64(lhs as f64 - rhs as f64) }
                    DataValue::F32(rhs) => { Value::from_f32(lhs as f32 - rhs) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs as f64 - rhs) }
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { Value::from_f64(lhs - rhs as f64) }
                    DataValue::I64(rhs) => { Value::from_f64(lhs - rhs as f64) }
                    DataValue::F32(rhs) => { Value::from_f64(lhs - rhs as f64) }
                    DataValue::F64(rhs) => { Value::from_f64(lhs - rhs) }
                }
            }
        }
    }
}

impl cmp::PartialEq for Value {
    fn eq(&self, rhs_value: &Self) -> bool {
        match self.value {
            DataValue::I32(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { lhs == rhs }
                    DataValue::I64(rhs) => { lhs as i64 == rhs }
                    DataValue::F32(rhs) => { lhs as f32 == rhs }
                    DataValue::F64(rhs) => { lhs as f64 == rhs }
                }
            }
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { lhs == rhs as i64 }
                    DataValue::I64(rhs) => { lhs == rhs }
                    DataValue::F32(rhs) => { lhs as f32 == rhs }
                    DataValue::F64(rhs) => { lhs as f64 == rhs }
                }
            }
            DataValue::F32(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { lhs == rhs as f32 }
                    DataValue::I64(rhs) => { lhs as f64 == rhs as f64 }
                    DataValue::F32(rhs) => { lhs as f32 == rhs }
                    DataValue::F64(rhs) => { lhs as f64 == rhs }
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { lhs == rhs as f64 }
                    DataValue::I64(rhs) => { lhs == rhs as f64 }
                    DataValue::F32(rhs) => { lhs == rhs as f64 }
                    DataValue::F64(rhs) => { lhs == rhs }
                }
            }
        }
    }
}

impl cmp::PartialOrd for Value {
    fn partial_cmp(&self, rhs_value: &Self) -> Option<Ordering> {
        match self.value {
            DataValue::I32(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { lhs.partial_cmp(&rhs) }
                    DataValue::I64(rhs) => { (lhs as i64).partial_cmp(&rhs) }
                    DataValue::F32(rhs) => { (lhs as f32).partial_cmp(&rhs) }
                    DataValue::F64(rhs) => { (lhs as f64).partial_cmp(&rhs) }
                }
            }
            DataValue::I64(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { lhs.partial_cmp(&(rhs as i64)) }
                    DataValue::I64(rhs) => { lhs.partial_cmp(&rhs) }
                    DataValue::F32(rhs) => { (lhs as f32).partial_cmp(&rhs) }
                    DataValue::F64(rhs) => { (lhs as f64).partial_cmp(&rhs) }
                }
            }
            DataValue::F32(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { lhs.partial_cmp(&(rhs as f32)) }
                    DataValue::I64(rhs) => { (lhs as f64).partial_cmp(&(rhs as f64)) }
                    DataValue::F32(rhs) => { (lhs as f32).partial_cmp(&rhs) }
                    DataValue::F64(rhs) => { (lhs as f64).partial_cmp(&rhs) }
                }
            }
            DataValue::F64(lhs) => {
                match rhs_value.value {
                    DataValue::I32(rhs) => { lhs.partial_cmp(&(rhs as f64)) }
                    DataValue::I64(rhs) => { lhs.partial_cmp(&(rhs as f64)) }
                    DataValue::F32(rhs) => { lhs.partial_cmp(&(rhs as f64)) }
                    DataValue::F64(rhs) => { lhs.partial_cmp(&rhs) }
                }
            }
        }
    }
}


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.value {
            DataValue::I32(v) => write!(f, "({}: i32)", v),
            DataValue::I64(v) => write!(f, "({}: i64)", v),
            DataValue::F32(v) => write!(f, "({}: f32)", v),
            DataValue::F64(v) => write!(f, "({}: f64)", v),
        }
    }
}
