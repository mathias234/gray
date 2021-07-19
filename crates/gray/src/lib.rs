pub mod bytecode;
pub mod interpreter;
pub mod parser;
pub mod compiler;
pub mod error_printer;

use std::rc::Rc;
use interpreter::interpreter::Interpreter;
use crate::parser::parser::{Parser, ParserError};
use crate::parser::lexer::{Lexer, LexerError};
use crate::compiler::compiler::{Compiler, CompilerError, NativeFunction};

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

pub fn load_file(file: &str, native_functions: Vec<NativeFunction>) -> Result<Interpreter, GrayError> {
    let token_stream = Lexer::lex_file(file)?;

    /*
    println!("\nLexer token stream");
    loop {
        let token = token_stream.next();
        if token.is_none() {
            break;
        }
        println!("{:?}", token.unwrap());
    }

    token_stream.reset();
     */

    let root_ast_node = Parser::parse(token_stream.1, &token_stream.0)?;

    //println!("\nParser AST Tree");
    root_ast_node.dump(0);

    let blocks = Compiler::compile(root_ast_node, native_functions.clone())?;

    let interpreter = Interpreter::new(blocks, token_stream.0, native_functions);


    return Ok(interpreter);
}

pub fn load_string(code: &str, native_functions: Vec<NativeFunction>) -> Result<Interpreter, GrayError> {
    let token_stream = Lexer::lex_string(code)?;

    let root_ast_node = Parser::parse(token_stream, code)?;

    let blocks = Compiler::compile(root_ast_node, native_functions.clone())?;

    let interpreter = Interpreter::new(blocks, Rc::from(code.to_string()), native_functions);

    return Ok(interpreter);
}
