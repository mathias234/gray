use super::interpreter::Interpreter;
use super::register::Register;

pub trait Instruction {
    fn execute(&self, interpereter: &mut Interpreter);
    fn to_string(&self) -> String;
}

pub struct LoadImmediate {
    value: i64,
}

impl LoadImmediate {
    pub fn new(value: i64) -> LoadImmediate {
        LoadImmediate { value }
    }
}

impl Instruction for LoadImmediate {
    fn execute(&self, interpereter: &mut Interpreter) {
        interpereter.set_accumulator(self.value);
    }

    fn to_string(&self) -> String {
        return format!("LoadImmediate value: {}", self.value);
    }
}

pub struct Store {
    register: Register,
}

impl Store {
    pub fn new(register: Register) -> Store {
        Store { register }
    }
}

impl Instruction for Store {
    fn execute(&self, interpereter: &mut Interpreter) {
        interpereter.set_register(&self.register, interpereter.get_accumulator());
    }

    fn to_string(&self) -> String {
        return format!("Store {}", self.register);
    }
}
