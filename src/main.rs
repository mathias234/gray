mod bytecode;

use bytecode::generator::Generator;
use bytecode::instructions::*;
use bytecode::interpreter::Interpreter;
use std::time::Instant;

fn main() {
    let mut blocks = Vec::new();

    let mut generator = Generator::new();
    code_block_1(&mut generator);

    blocks.push(generator.get_block());

    let mut generator = Generator::new();
    code_block_2(&mut generator);

    blocks.push(generator.get_block());

    let mut interpreter = Interpreter::new(blocks);

    let now = Instant::now();

    interpreter.run();

    println!{"Execution took {}ms", now.elapsed().as_millis()}
}

fn code_block_1(generator: &mut Generator) {

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
    generator.emit(JumpNotZero::new_boxed(Some(loop_start)));

    generator.emit(Call::new_boxed(1));

    generator.emit(Return::new_boxed());
}

fn code_block_2(generator: &mut Generator) {

    let iterator_register = generator.next_free_register();
    generator.emit(LoadImmediate::new_boxed(0));
    generator.emit(Store::new_boxed(iterator_register));


    let target_value_register = generator.next_free_register();
    generator.emit(LoadImmediate::new_boxed(3));
    generator.emit(Store::new_boxed(target_value_register));

    let increment_register = generator.next_free_register();
    generator.emit(LoadImmediate::new_boxed(1));
    generator.emit(Store::new_boxed(increment_register));

    let loop_start = generator.make_label();

    generator.emit(LoadRegister::new_boxed(iterator_register));
    generator.emit(Add::new_boxed(increment_register));
    generator.emit(Store::new_boxed(iterator_register));

    generator.emit(CompareEq::new_boxed(target_value_register));
    generator.emit(JumpZero::new_boxed(Some(loop_start)));
    generator.emit(Return::new_boxed());
}