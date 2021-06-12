mod bytecode;

use bytecode::generator::Generator;
use bytecode::instructions::*;
use bytecode::interpreter::Interpreter;
use std::time::Instant;

fn main() {
    let mut blocks = Vec::new();

    let mut generator = Generator::new();
    code_block_0(&mut generator);

    blocks.push(generator.get_block());

    let mut generator = Generator::new();
    code_block_1(&mut generator);

    blocks.push(generator.get_block());

    let mut interpreter = Interpreter::new(blocks);

    let now = Instant::now();

    interpreter.run();

    println! {"Execution took {}ms", now.elapsed().as_millis()}
}

fn code_block_0(generator: &mut Generator) {
    let iterator_register = generator.next_free_register();

    generator.emit(LoadImmediate::new_boxed(3));
    generator.emit(Store::new_boxed(iterator_register));

    let decrement_register = generator.next_free_register();

    generator.emit(LoadImmediate::new_boxed(1));
    generator.emit(Store::new_boxed(decrement_register));

    let loop_start = generator.make_label();

    generator.emit(LoadRegister::new_boxed(iterator_register));
    generator.emit(Subtract::new_boxed(decrement_register));

    generator.emit(Store::new_boxed(iterator_register));

    generator.emit(LoadRegister::new_boxed(iterator_register));
    generator.emit(JumpNotZero::new_boxed(loop_start));

    let arg1_register = generator.next_free_register();
    generator.emit(LoadImmediate::new_boxed(3));
    generator.emit(Store::new_boxed(arg1_register));

    generator.emit(Call::new_boxed(1, Some(vec![arg1_register])));

    generator.emit(Return::new_boxed());
}

fn code_block_1(generator: &mut Generator) {
    generator.emit(LoadArgument::new_boxed(0));
    let argument_register = generator.next_free_register();

    generator.emit(Store::new_boxed(argument_register));

    generator.emit(LoadImmediate::new_boxed(10000));

    generator.emit(CompareEq::new_boxed(argument_register));

    let jump_placeholder = generator.make_label();

    generator.emit(LoadImmediate::new_boxed(1));

    generator.emit(Add::new_boxed(argument_register));

    let result_register = generator.next_free_register();
    generator.emit(Store::new_boxed(result_register));

    generator.emit(Call::new_boxed(1, Some(vec![result_register])));

    let mut exit_label = generator.make_label();
    exit_label.position += 1;
    generator.emit_at(JumpNotZero::new_boxed(exit_label), &jump_placeholder);

    generator.emit(Return::new_boxed());
}
