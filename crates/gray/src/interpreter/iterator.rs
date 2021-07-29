use crate::interpreter::array::Array;
use crate::interpreter::value::Value;
use core::fmt;
use std::cell::RefCell;
use std::fmt::Formatter;
use std::rc::Rc;

pub trait Iterator {
    fn pop_next(&mut self) -> Option<Value>;
    fn empty(&self) -> bool;
}

#[derive(Clone)]
pub struct IteratorHolder {
    pub iterator: Rc<RefCell<dyn Iterator>>,
}

impl std::fmt::Debug for IteratorHolder {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "(Iterator)")
    }
}

#[derive(Clone, Debug)]
pub struct RangeIterator {
    pub from: i64,
    pub to: i64,
    pub index: i64,
}

#[derive(Clone, Debug)]
pub struct ArrayIterator {
    pub index: i64,
    pub array: Array,
}

impl Iterator for RangeIterator {
    fn pop_next(&mut self) -> Option<Value> {
        if self.index >= self.to {
            return None;
        }

        let result = Some(Value::from_i64(self.from + self.index));
        self.index += 1;

        result
    }

    fn empty(&self) -> bool {
        if self.index >= self.to {
            return true;
        }

        return false;
    }
}

impl Iterator for ArrayIterator {
    fn pop_next(&mut self) -> Option<Value> {
        if self.index >= self.array.len() as i64 {
            return None;
        }

        let result = Some(self.array.get(self.index as usize));
        self.index += 1;

        result
    }

    fn empty(&self) -> bool {
        if self.index >= self.array.len() as i64 {
            return true;
        }

        return false;
    }
}
