use crate::interpreter::value::Value;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;

#[derive(Clone)]
pub struct Array {
    array: Rc<RefCell<Vec<Value>>>,
}

impl Array {
    pub fn new() -> Array {
        Array {
            array: Rc::new(RefCell::new(Vec::new())),
        }
    }

    pub fn push(&mut self, value: Value) {
        self.array.borrow_mut().push(value);
    }

    pub fn insert(&mut self, index: usize, value: Value) {
        self.array.borrow_mut().insert(index, value);
    }

    pub fn get(&self, index: usize) -> Value {
        self.array.borrow()[index].clone()
    }

    pub fn len(&self) -> usize {
        self.array.borrow().len()
    }
}

impl fmt::Display for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self,f)?;
        Ok({})
    }
}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let array = self.array.borrow();

        f.write_str("[")?;
        for i in 0..array.len() {
            f.write_str(&format!("{}", array[i]))?;
            if i < array.len() - 1 {
                f.write_str(", ")?;
            }
        }
        f.write_str("]")?;

        Ok({})
    }
}