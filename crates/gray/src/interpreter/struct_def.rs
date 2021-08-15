use std::rc::Rc;

use super::{object::Object, value::Value};

#[derive(Clone, Debug)]
pub struct StructDef {
    variables: Vec<String>,
    functions: Vec<(String, String)>,
}

impl StructDef {
    pub fn new() -> StructDef {
        StructDef {
            variables: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn add_variable(&mut self, name: String) {
        self.variables.push(name)
    }

    pub fn add_function(&mut self, name: String, full_name: String) {
        self.functions.push((name, full_name))
    }

    pub fn create_object(&self) -> Object {
        let mut obj = Object::new();

        for variable in &self.variables {
            obj.declare(Rc::from(variable.clone()), &Value::from_undefined());
        }

        for function in &self.functions {
            obj.declare(
                Rc::from(function.0.clone()),
                &Value::from_function(Rc::from(function.1.clone())),
            );
        }

        obj
    }
}
