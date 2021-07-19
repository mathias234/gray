use gray::interpreter::interpreter::ExecutionContext;
use gray::interpreter::value::Value;
use gray::interpreter::function_pointer::FunctionArgs;
use gray::compiler::compiler::NativeFunction;

pub fn load_functions(functions: &mut Vec<NativeFunction>) {
    functions.push(NativeFunction::new(vec!["array".to_string()], String::from("push"), array_push));
    functions.push(NativeFunction::new(vec!["array".to_string()], String::from("get"), array_get));
    functions.push(NativeFunction::new(vec!["array".to_string()], String::from("len"), array_len));
}

fn array_push(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let mut array = args.get_next_array(context);

    let mut pushed = 0;
    for _ in 0..args.len() {
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