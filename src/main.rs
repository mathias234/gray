pub mod bytecode;
pub mod interpreter;
pub mod parser;
pub mod compiler;

use interpreter::interpreter::Interpreter;
use crate::parser::parser::{Parser, ParserError};
use crate::parser::lexer::{Lexer, LexerError};
use crate::compiler::compiler::{Compiler, CompilerError};
use crate::interpreter::value::{Value, DataValue};
use std::time::Instant;

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
    let now = Instant::now();

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
    interpreter.set_native_function(String::from("format"), format_to_value);

    interpreter.run(String::from("entry"));

    println! {"Execution took {}ms", now.elapsed().as_millis()}


    return Ok({});
}

fn print_function(args: Vec<Value>) -> Value {

    println!("{}", value_to_string(&format_to_value(args)));

    Value::from_i64(0)
}

fn format_to_value(args: Vec<Value>) -> Value {
    let format_str = &args[0];
    let format_str = match format_str.get_data_value() {
        DataValue::String(str) => str,
        _ => panic!("Only strings can be used as format string"),
    };

    let mut arg_idx = 1;

    let mut formatted_string = String::new();

    let mut chars = format_str.chars();
    loop {
        let char = chars.next();
        if char.is_none() {
            break;
        }

        match char.unwrap() {
            '{' => {
                chars.next();

                let value_formatted = value_to_string(&args[arg_idx]);
                formatted_string.push_str(&value_formatted);

                arg_idx += 1;
            }
            c => {
                formatted_string.push(c);
            }
        }
    }

    Value::from_string(formatted_string)
}

fn value_to_string(args: &Value) -> String {
    match args.get_data_value() {
        DataValue::F64(float_value) => format!("{}", float_value),
        DataValue::I64(int_value) => format!("{}", int_value),
        DataValue::Object(object) => format!("{:?}", object),
        DataValue::String(string) => format!("{}", string),
    }
}