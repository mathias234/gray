mod bytecode;

use bytecode::generator::Generator;
use bytecode::instructions::*;
use bytecode::interpreter::Interpreter;
use std::time::Instant;

fn main() {
    let mut generator = Generator::new();

    let iterator_register = generator.next_free_register();
    generator.emit(LoadImmediate::new_boxed(100000));
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

    let mut interpreter = Interpreter::new(generator.get_block());

    let now = Instant::now();

    Interpreter::run(&mut interpreter);

    println!{"Execution took {}ms", now.elapsed().as_millis()}
}
