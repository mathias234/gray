use std::collections::HashMap;
use crate::interpreter::interpreter::VariableHandle;
use crate::bytecode::instructions::other::Instruction;
use crate::bytecode::label::Label;

#[derive(Copy, Clone, PartialEq)]
pub struct CodeSegment {
    pub start_x: usize,
    pub start_y: usize,
    pub end_x: usize,
    pub end_y: usize,
}

impl CodeSegment {
    pub fn new(start_x: usize, start_y: usize, end_x: usize, end_y: usize) -> CodeSegment {
        CodeSegment {
            start_x,
            start_y,
            end_x,
            end_y,
        }
    }
}

impl std::fmt::Debug for CodeSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}:{} - {}:{}",
            self.start_y, self.start_x, self.end_y, self.end_x
        ))
    }
}

pub struct CodeBlock {
    instructions: Vec<Box<dyn Instruction>>,
    pub code_mapping: Vec<CodeSegment>, // Maps instruction to a physical location in the original code
    pub capture_locals: bool,
    pub variable_handles: HashMap<String, VariableHandle>,
    pub last_handle: VariableHandle,
}

impl CodeBlock {
    pub fn new(capture_locals: bool) -> CodeBlock {
        CodeBlock {
            instructions: Vec::new(),
            code_mapping: Vec::new(),
            capture_locals,
            variable_handles: HashMap::new(),
            last_handle: 0
        }
    }

    pub fn add_instruction(&mut self, instruction: Box<dyn Instruction>, segment: CodeSegment) {
        self.code_mapping.push(segment);
        self.instructions.push(instruction);
    }

    pub fn set_instruction_at(
        &mut self,
        instruction: Box<dyn Instruction>,
        label: &Label,
        segment: CodeSegment,
    ) {
        self.code_mapping[label.position] = segment;
        self.instructions[label.position] = instruction;
    }

    pub fn get_instructions(&self) -> &Vec<Box<dyn Instruction>> {
        &self.instructions
    }

    pub fn next_variable_handle(&mut self, variable: &str) -> VariableHandle {
        if self.variable_handles.contains_key(variable) {
            return self.variable_handles[variable];
        }

        self.variable_handles
            .insert(variable.to_string(), self.last_handle);
        let handle = self.last_handle;

        self.last_handle += 1;

        return handle;
    }

    pub fn get_variable_handle(&self, variable: &str) -> Option<VariableHandle> {
        if self.variable_handles.contains_key(variable) {
            return Some(self.variable_handles[variable]);
        }

        None
    }
}
