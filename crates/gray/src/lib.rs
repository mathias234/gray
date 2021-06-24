pub mod bytecode;
pub mod interpreter;
pub mod parser;
pub mod compiler;

use interpreter::interpreter::Interpreter;
use crate::parser::parser::{Parser, ParserError};
use crate::parser::lexer::{Lexer, LexerError};
use crate::compiler::compiler::{Compiler, CompilerError};
use std::time::Instant;

#[derive(Debug)]
pub enum GrayError {
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

pub fn load_file(file: &str) -> Result<Interpreter, GrayError> {
    let now = Instant::now();

    let mut token_stream = Lexer::lex_file(file)?;

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

    let interpreter = Interpreter::new(blocks);

    println! {"Execution took {}ms", now.elapsed().as_millis()}


    return Ok(interpreter);
}
