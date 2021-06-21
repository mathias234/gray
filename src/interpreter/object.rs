use std::collections::HashMap;
use crate::interpreter::value::Value;
use std::rc::Rc;

#[derive(Clone)]
pub struct Object {
    variables: HashMap<Rc<String>, Value>,
}

impl Object {
    pub fn new() -> Object {
        Object {
            variables: HashMap::new(),
        }
    }

    pub fn set(&mut self, name: Rc<String>, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn get_mut(&mut self, name: Rc<String>) -> Option<&mut Value> {
        self.variables.get_mut(&name)
    }

    pub fn get(&self, name: Rc<String>) -> Option<&Value> {
        self.variables.get(&name)
    }
}