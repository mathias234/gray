pub mod bytecode;
pub mod interpreter;
pub mod parser;

use bytecode::generator::Generator;
use bytecode::instructions::*;
use interpreter::interpreter::Value;
use interpreter::interpreter::Interpreter;
use parser::lexer::Lexer;

fn main() {
    let token_stream = Lexer::lex_file("./test.gray");

    let token_stream = match token_stream {
        Ok(t) => t,
        Err(e) => match e {
            _ => panic!("Lexer failed with error: {:#?}", e),
        },
    };

    for token in token_stream.tokens {
        println!("{:#?}", token);
    }

    return;
    let mut blocks = Vec::new();

    let mut generator = Generator::new();
    code_block_0(&mut generator);

    blocks.push(generator.get_block());

    let mut generator = Generator::new();
    code_block_1(&mut generator);

    blocks.push(generator.get_block());

    let mut interpreter = Interpreter::new(blocks);

    interpreter.run();
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
