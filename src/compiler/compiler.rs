use crate::parser::parser::{ASTNode, ASTType};
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::generator::Generator;
use crate::bytecode::instructions::{Return, Call, LoadImmediate, Store, Add, SetVariable};
use std::collections::HashMap;
use crate::bytecode::register::Register;
use crate::interpreter::value::Value;

#[derive(Debug)]
pub enum CompilerError {
    UnexpectedASTNode(ASTNode)
}

pub struct Compiler {
    blocks: HashMap<String, CodeBlock>,
}

impl Compiler {
    pub fn compile(root_node: ASTNode) -> Result<HashMap<String, CodeBlock>, CompilerError> {
        let mut compiler = Compiler {
            blocks: HashMap::new(),
        };

        compiler.compile_program_root(&root_node)?;


        Ok(compiler.blocks)
    }

    fn compile_program_root(&mut self, root: &ASTNode) -> Result<(), CompilerError> {
        for child in &root.children {
            match &child.ast_type {
                ASTType::Structure(name) => self.compile_structure(name, child),
                ASTType::Function(name) => self.compile_function(name, child),
                _ => Err(CompilerError::UnexpectedASTNode(child.clone())),
            }?;
        }

        Ok({})
    }

    fn compile_structure(&mut self, name: &str, node: &ASTNode) -> Result<(), CompilerError> {
        for child in &node.children {
            match &child.ast_type {
                ASTType::Function(function_name) => self.compile_function(&format!("{}::{}", name, function_name), child),
                _ => Err(CompilerError::UnexpectedASTNode(child.clone())),
            }?;
        }

        Ok({})
    }

    fn compile_function(&mut self, name: &str, node: &ASTNode) -> Result<(), CompilerError> {
        if node.children.len() > 0 {
            self.compile_scope(name, &node.children[0])?;
        }
        Ok({})
    }

    fn compile_scope(&mut self, name: &str, node: &ASTNode) -> Result<(), CompilerError> {
        let mut generator = Generator::new();

        for child in &node.children {
            match &child.ast_type {
                ASTType::FunctionCall(call) => {
                    generator.emit(Call::new_boxed(&*call, None));
                }
                ASTType::VariableDeclaration(variable) => {
                    self.compile_variable_declaration(variable, &mut generator, child);
                }
                _ => {}
            }
        }

        generator.emit(Return::new_boxed());

        self.blocks.insert(String::from(name), generator.block);

        Ok({})
    }

    fn compile_variable_declaration(&mut self, variable: &String, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        self.compile_expression(generator, &node.children[0])?;
        generator.emit(SetVariable::new_boxed(variable.clone()));

        Ok({})
    }

    fn compile_expression(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let child = &node.children[0];
        match &child.ast_type {
            ASTType::MathExpression => self.compile_math_expression(generator, child),
            _ => Err(CompilerError::UnexpectedASTNode(child.clone())),
        }
    }

    fn compile_math_expression(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let rhs_register = self.compile_value(generator, &node.children[2])?;
        self.compile_value_to_accumulator(generator, &node.children[0])?;

        generator.emit(Add::new_boxed(rhs_register));

        Ok({})
    }

    fn compile_value(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<Register, CompilerError> {
        let value_register = generator.next_free_register();

        self.compile_value_to_accumulator(generator, node)?;

        generator.emit(Store::new_boxed(value_register));

        Ok(value_register)
    }

    fn compile_value_to_accumulator(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let value = match node.ast_type {
            ASTType::IntegerValue(value) => Ok(Value::from_i64(value)),
            ASTType::FloatValue(value) => Ok(Value::from_f64(value)),
            _ => Err(CompilerError::UnexpectedASTNode(node.clone())),
        }?;

        Ok(generator.emit(LoadImmediate::new_boxed(value)))
    }
}