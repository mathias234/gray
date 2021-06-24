use gray::GrayError;
use built_in_functions::declare_functions;

fn main() -> Result<(), GrayError> {
    let mut interpreter = gray::load_file("./test.gray")?;

    declare_functions(&mut interpreter);

    interpreter.run(String::from("entry"));

    Ok({})
}