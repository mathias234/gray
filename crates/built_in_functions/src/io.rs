use gray::interpreter::interpreter::{Interpreter, ExecutionContext};
use gray::interpreter::value::{Value, Pointer};
use std::rc::Rc;
use std::io::Read;
use gray::interpreter::function_pointer::FunctionArgs;
use std::any::Any;

pub fn load_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(vec!["fs"], String::from("open"), fs_open);
    interpreter.set_native_function(vec!["fs"], String::from("read_to_string"), fs_read_to_string);
    interpreter.set_native_function(vec!["io"], String::from("read_line"), io_read_line);
}

pub fn fs_open(_: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let file_name = args.get_next_string();

    let file = std::fs::File::open(file_name.as_str()).expect("Failed to open file");

    Value::to_pointer(file)
}

pub fn fs_read_to_string(_: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let mut file = value_to_file(args.get_next_pointer());

    let mut buffer = String::new();
    file.read_to_string(&mut buffer).expect("Error reading files contents");

    Value::from_string(Rc::from(buffer))
}

pub fn io_read_line(_: &ExecutionContext, _: FunctionArgs) -> Value {
    let mut input = String::new();
    std::io::stdin().read_line(&mut input).expect("Error reading for stdin");

    Value::from_string(Rc::new(input.trim().to_string()))
}

fn value_to_file(p: Pointer<dyn Any>) -> std::fs::File {
    let p = p.borrow();
    if let Some(file) = p.downcast_ref::<std::fs::File>() {
        file.try_clone().unwrap()
    } else {
        panic!("Expected pointer to a file from fs::open");
    }
}