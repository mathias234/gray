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

pub fn fs_open(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let file_name = args.get_next_string(context);

    match std::fs::File::open(file_name.as_str()) {
        Ok(file) => return Value::to_pointer(file),
        Err(e) => context.throw_error(&format!("Failed to open file `{}`", e))
    }
}

pub fn fs_read_to_string(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let mut file = value_to_file(context, args.get_next_pointer(context));

    let mut buffer = String::new();

    match file.read_to_string(&mut buffer)
    {
        Ok(_) => return Value::from_string(Rc::from(buffer)),
        Err(e) => context.throw_error(&format!("Error reading files contents `{}`", e))
    }
}

pub fn io_read_line(context: &ExecutionContext, _: FunctionArgs) -> Value {
    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => Value::from_string(Rc::new(input.trim().to_string())),
        Err(e) => context.throw_error(&format!("Error reading for stdin `{}`", e))
    }
}

fn value_to_file(context: &ExecutionContext, p: Pointer<dyn Any>) -> std::fs::File {
    let p = p.borrow();
    if let Some(file) = p.downcast_ref::<std::fs::File>() {
        file.try_clone().unwrap()
    } else {
        context.throw_error("Expected pointer to a file object");
    }
}