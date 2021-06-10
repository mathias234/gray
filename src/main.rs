mod bytecode;

use bytecode::generator::Generator;
use bytecode::instructions::*;
use bytecode::interpreter::Interpreter;

fn main() {
    let mut generator = Generator::new();

    generator.emit(&LoadImmediate::new(100));

    let dest = generator.next_free_register();
    generator.emit(&Store::new(dest));

    let mut interpreter = Interpreter::new();
}
