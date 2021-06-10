use super::register::Register;

pub struct Interpreter {
    accumulator: i64,
    registers: Vec<i64>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            accumulator: 0,
            registers: Vec::new(),
        }
    }
    pub fn set_accumulator(&mut self, value: i64) {
        self.accumulator = value;
    }
    pub fn get_accumulator(&self) -> i64 {
        self.accumulator
    }

    pub fn set_register(&mut self, register: &Register, value: i64) {
        self.registers[register.index] = value;
    }

    pub fn get_register(&self, register: &Register) -> i64 {
        self.registers[register.index]
    }
}
