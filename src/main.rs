pub mod bytecode;
pub mod interpreter;
pub mod parser;
pub mod compiler;

use interpreter::interpreter::Interpreter;
use crate::parser::parser::{Parser, ParserError};
use crate::parser::lexer::{Lexer, LexerError};
use crate::compiler::compiler::{Compiler, CompilerError};
use crate::interpreter::value::{Value, DataValue};

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

    interpreter.set_native_function(String::from("print"), print_function);

    interpreter.run(String::from("entry"));

    return Ok({});
}

fn print_function(args: Vec<Value>) {
    for arg in args {
        let pretty_printed = match arg.get_data_value() {
            DataValue::F64(float_value) => format!("{}", float_value),
            DataValue::I64(int_value) => format!("{}", int_value),
            DataValue::Object(object) => format!("{:?}", object),
        };

        print!("{} ", pretty_printed);
    }
    println!();
}