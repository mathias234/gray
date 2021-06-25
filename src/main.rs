use gray::GrayError;
use built_in_functions::declare_functions;
use std::time::Instant;

fn main() -> Result<(), GrayError> {
    let now = Instant::now();

    let mut interpreter = gray::load_file("./test.gray")?;

    declare_functions(&mut interpreter);

    interpreter.run(None);

    println! {"Execution took {}ms", now.elapsed().as_millis()}

    Ok({})
}