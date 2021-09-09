use crate::bytecode::code_block::{CodeBlock, CodeSegment};
use crate::bytecode::instructions::object::NotAnInstruction;
use crate::bytecode::instructions::other::{DeclareVariable, Instruction, LoadImmediate};
use crate::bytecode::label::Label;
use crate::bytecode::register::Register;
use crate::compiler::compiler::NativeFunction;
use crate::interpreter::interpreter::VariableHandle;
use crate::interpreter::value::Value;
use std::rc::Rc;

pub struct Generator {
    register_index: usize,
    released_registers: Vec<Register>,
    pub block: CodeBlock,
    lambda_index: usize,
}

impl Generator {
    pub fn new(
        native_functions: &Vec<NativeFunction>,
        capture_locals: bool,
        parent_generator: Option<&Generator>,
    ) -> Generator {
        let mut generator = Generator {
            register_index: 0,
            released_registers: Vec::new(),
            block: CodeBlock::new(capture_locals),
            lambda_index: 0,
        };

        if capture_locals && parent_generator.is_some() {
            let parent = parent_generator.unwrap();
            generator.block.variable_handles = parent.block.variable_handles.clone();
            generator.block.last_handle = parent.block.last_handle;
        }

        for func in native_functions {
            let handle = generator.next_variable_handle(&func.full_name());
            generator.emit(
                LoadImmediate::new_boxed(Value::from_function(Rc::new(func.full_name()))),
                CodeSegment::new(1, 1, 1, 1),
            );
            generator.emit(
                DeclareVariable::new_boxed(handle),
                CodeSegment::new(1, 1, 1, 1),
            );
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
        return self.block.next_variable_handle(variable);
    }

    pub fn next_lambda_handle(&mut self) -> String {
        let handle = format!("__LambdaFunction[{}]", self.lambda_index);
        self.lambda_index += 1;
        handle
    }

    pub fn emit(&mut self, instruction: Box<dyn Instruction>, segment: CodeSegment) {
        self.block.add_instruction(instruction, segment);
    }

    pub fn emit_at(
        &mut self,
        instruction: Box<dyn Instruction>,
        label: &Label,
        segment: CodeSegment,
    ) {
        self.block.set_instruction_at(instruction, label, segment);
    }
}
