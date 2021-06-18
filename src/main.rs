pub mod bytecode;
pub mod interpreter;
pub mod parser;
pub mod compiler;

use bytecode::generator::Generator;
use bytecode::instructions::*;
use interpreter::interpreter::Value;
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

fn code_block_0(generator: &mut Generator) {
    let arg1_register = generator.next_free_register();
    generator.emit(LoadImmediate::new_boxed(Value::from_i32(0)));
    generator.emit(Store::new_boxed(arg1_register));

    generator.emit(Call::new_boxed(1, Some(vec![arg1_register])));

    generator.emit(Return::new_boxed());
}

fn code_block_1(generator: &mut Generator) {
    generator.emit(LoadArgument::new_boxed(0));
    let argument_register = generator.next_free_register();

    generator.emit(Store::new_boxed(argument_register));

    generator.emit(LoadImmediate::new_boxed(Value::from_i32(100_000)));

    generator.emit(CompareEq::new_boxed(argument_register));

    let jump_placeholder = generator.make_label();

    generator.emit(LoadImmediate::new_boxed(Value::from_i32(1)));

    generator.emit(Add::new_boxed(argument_register));

    let result_register = generator.next_free_register();
    generator.emit(Store::new_boxed(result_register));

    generator.emit(Call::new_boxed(1, Some(vec![result_register])));

    let mut exit_label = generator.make_label();
    exit_label.position += 1;
    generator.emit_at(JumpNotZero::new_boxed(exit_label), &jump_placeholder);

    generator.emit(Return::new_boxed());
}
