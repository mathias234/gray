use crate::parser::parser::ASTNode;
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::generator::Generator;
use crate::bytecode::instructions::Return;

pub struct Compiler {
    blocks: Vec<CodeBlock>,
    root_node: ASTNode
}

impl Compiler {
    pub fn compile(node: ASTNode) -> Vec<CodeBlock> {
        let compiler = Compiler {
            blocks: Vec::new(),
            root_node: node,
        };

        compiler.blocks
    }

    fn compile_scope(&mut self) {
        let mut generator = Generator::new();



        self.blocks.push(generator.block);
    }
}