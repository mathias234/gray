mod bytecode;

use bytecode::generator::Generator;
use bytecode::instructions::*;
use bytecode::interpreter::Interpreter;

fn main() {
    let mut generator = Generator::new();

    generator.emit(LoadImmediate::new_boxed(100));

    let dest = generator.next_free_register();
    generator.emit(Store::new_boxed(dest));

    generator.emit(LoadImmediate::new_boxed(50));
    generator.emit(Add::new_boxed(dest));

    let dest = generator.next_free_register();
    generator.emit(Store::new_boxed(dest));

    let mut interpreter = Interpreter::new(generator.get_block());

    Interpreter::run(&mut interpreter);
}
