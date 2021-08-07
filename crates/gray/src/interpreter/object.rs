use crate::interpreter::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub struct Object {
    pub variables: Rc<RefCell<HashMap<Rc<String>, Value>>>,
}

impl Object {
    pub fn new() -> Object {
        Object {
            variables: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn declare(&mut self, name: Rc<String>, value: &Value) {
        self.variables.borrow_mut().insert(name, value.clone());
    }

    pub fn set(&mut self, name: Rc<String>, value: &Value) -> bool {
        let mut variables = self.variables.borrow_mut();
        if variables.contains_key(&name) {
            variables.insert(name, value.clone());
            return true;
        }

        return false;
    }

    pub fn get(&self, name: Rc<String>) -> Option<Value> {
        let variables_borrowed = self.variables.borrow();
        let result = variables_borrowed.get(&name);
        if result.is_some() {
            return Some(result.unwrap().clone());
        }

        None
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[ ")?;

        let count = self.variables.borrow().len();
        let mut idx = 0;
        for (k, v) in self.variables.borrow().clone() {
            f.write_fmt(format_args!("{}: {}", k, v.to_string()))?;
            if idx < count - 1 {
                f.write_str(", ")?;
            }

            idx += 1;
        }
        f.write_str(" ]")?;

        Ok({})
    }
}
