use crate::parser::parser::{ASTNode, ASTType};
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::generator::Generator;
use crate::bytecode::instructions::Return;
use std::collections::HashMap;

#[derive(Debug)]
pub enum CompilerError {
    UnexpectedASTNode(ASTNode)
}

struct CompilerBlock {
    block_index: usize,
    code_block: CodeBlock,
}

impl CompilerBlock {
    pub fn new(compiler: &mut Compiler, block: CodeBlock) -> CompilerBlock {
        let compiler_block = CompilerBlock {
            block_index: compiler.last_block_index,
            code_block: block,
        };

        compiler.last_block_index += 1;

        return compiler_block;
    }
}

pub struct Compiler {
    blocks: HashMap<String, CompilerBlock>,
    last_block_index: usize,
}

impl Compiler {
    pub fn compile(root_node: ASTNode) -> Result<Vec<CodeBlock>, CompilerError> {
        let mut compiler = Compiler {
            blocks: HashMap::new(),
            last_block_index: 0,
        };

        compiler.compile_program_root(&root_node)?;

        let mut blocks = Vec::with_capacity(compiler.blocks.len());

        for (block_name, compiler_block) in compiler.blocks {
            blocks.insert(compiler_block.block_index, compiler_block.code_block);
        }

        Ok(blocks)
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

        let compiler_block = CompilerBlock::new(self, generator.block);
        self.blocks.insert(String::from(name), compiler_block);
        Ok({})
    }
}