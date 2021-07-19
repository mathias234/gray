use crate::bytecode::code_block::{CodeBlock, CodeSegment};
use crate::bytecode::label::Label;
use crate::bytecode::instructions::other::{Instruction, DeclareVariable, LoadImmediate};
use crate::bytecode::register::Register;
use crate::bytecode::instructions::object::NotAnInstruction;
use crate::interpreter::interpreter::VariableHandle;
use std::collections::HashMap;
use crate::compiler::compiler::NativeFunction;
use crate::interpreter::value::Value;
use std::rc::Rc;

pub struct Generator {
    register_index: usize,
    released_registers: Vec<Register>,
    pub block: CodeBlock,
    variable_handles: HashMap<String, VariableHandle>,
    last_handle: VariableHandle,
}

impl Generator {
    pub fn new(native_functions: &Vec<NativeFunction>) -> Generator {
        let mut generator = Generator {
            register_index: 0,
            released_registers: Vec::new(),
            block: CodeBlock::new(),
            variable_handles: HashMap::new(),
            last_handle: 0,
        };

        for func in native_functions {
            let handle = generator.next_variable_handle(&func.full_name());
            generator.emit(LoadImmediate::new_boxed(Value::from_function(Rc::new(func.full_name()))), CodeSegment::new(1, 1, 1, 1));
            generator.emit(DeclareVariable::new_boxed(handle), CodeSegment::new(1, 1, 1, 1));
        }

        generator
    }

    pub fn next_free_register(&mut self) -> Register {
        if self.released_registers.len() > 0 {
            let register = self.released_registers.pop().unwrap();
            return register;
        }

        let result = Register::new(self.register_index);
        self.register_index += 1;
        result
    }

    pub fn release_register(&mut self, register: Register) {
        self.released_registers.push(register);
    }

    pub fn make_label(&self) -> Label {
        let pos = self.block.get_instructions().len();
        Label::new(pos)
    }

    pub fn make_instruction_holder(&mut self) -> Label {
        let label = self.make_label();

        self.emit(NotAnInstruction::new_boxed(), CodeSegment::new(1, 1, 1, 1));

        return label;
    }

    pub fn next_variable_handle(&mut self, variable: &str) -> VariableHandle {
        if self.variable_handles.contains_key(variable) {
            return self.variable_handles[variable];
        }

        self.variable_handles.insert(variable.to_string(), self.last_handle);
        let handle = self.last_handle;

        self.last_handle += 1;

        return handle;
    }

    pub fn emit(&mut self, instruction: Box<dyn Instruction>, segment: CodeSegment) {
        self.block.add_instruction(instruction, segment);
    }

    pub fn emit_at(&mut self, instruction: Box<dyn Instruction>, label: &Label, segment: CodeSegment) {
        self.block.set_instruction_at(instruction, label, segment);
    }
}
