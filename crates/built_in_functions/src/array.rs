use gray::interpreter::interpreter::{Interpreter, ExecutionContext};
use gray::interpreter::value::Value;
use gray::interpreter::function_pointer::FunctionArgs;

pub fn load_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(vec!["array"], String::from("push"), array_push);
    interpreter.set_native_function(vec!["array"], String::from("get"), array_get);
    interpreter.set_native_function(vec!["array"], String::from("len"), array_len);
}

fn array_push(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let mut array = args.get_next_array(context);

    let mut pushed = 0;
    for _ in 0..args.len() {
        println!("Push, push push");
        array.push(args.get_next(context).clone());
        pushed += 1;
    }

    Value::from_i64(pushed as i64)
}

fn array_get(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let array = args.get_next_array(context);
    let index = args.get_next_i64(context) as usize;
    array.get(index)
}

fn array_len(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    Value::from_i64(args.get_next_array(context).len() as i64)
}