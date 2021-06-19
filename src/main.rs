pub mod bytecode;
pub mod interpreter;
pub mod parser;
pub mod compiler;

use interpreter::interpreter::Interpreter;
use crate::parser::parser::{Parser, ParserError};
use crate::parser::lexer::{Lexer, LexerError};
use crate::compiler::compiler::{Compiler, CompilerError};

#[derive(Debug)]
enum GrayError {
    ParserError(ParserError),
    LexerError(LexerError),
    CompilerError(CompilerError),
}

impl From<LexerError> for GrayError {
    fn from(error: LexerError) -> Self {
        GrayError::LexerError(error)
    }
}

impl From<ParserError> for GrayError {
    fn from(error: ParserError) -> Self {
        GrayError::ParserError(error)
    }
}

impl From<CompilerError> for GrayError {
    fn from(error: CompilerError) -> Self {
        GrayError::CompilerError(error)
    }
}

fn main() -> Result<(), GrayError> {
    let mut token_stream = Lexer::lex_file("./test.gray")?;

    println!("\nLexer token stream");
    loop {
        let token = token_stream.next();
        if token.is_none() {
            break;
        }
        println!("{:?}", token.unwrap());
    }

    token_stream.reset();

    let root_ast_node = Parser::parse(token_stream)?;

    println!("\nParser AST Tree");

    root_ast_node.dump(0);

    let blocks = Compiler::compile(root_ast_node)?;

    let mut interpreter = Interpreter::new(blocks);

    interpreter.run();

    return Ok({});
}