use crate::parser::parser::{ASTNode, ASTType};
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::generator::Generator;
use crate::bytecode::instructions::Return;
use std::collections::HashMap;

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
                ASTType::Structure(name) => self.compile_structure(child),
                _ => Err(CompilerError::UnexpectedASTNode(child.clone())),
            }?;
        }

        Ok({})
    }

    fn compile_structure(&mut self, node: &ASTNode) -> Result<(), CompilerError> {
        for child in &node.children {
            match &child.ast_type {
                ASTType::Function(function_name) => self.compile_function(function_name, child),
                _ => Err(CompilerError::UnexpectedASTNode(child.clone())),
            }?;
        }

        Ok({})
    }

    fn compile_trait(&mut self, node: &ASTNode) -> Result<(), CompilerError> {
        Ok({})
    }

    fn compile_function(&mut self, name: &str, node: &ASTNode) -> Result<(), CompilerError> {
        let mut generator = Generator::new();

        generator.emit(Return::new_boxed());

        self.blocks.insert(String::from(name), generator.block);
        Ok({})
    }
}