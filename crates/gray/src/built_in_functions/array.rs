use crate::interpreter::interpreter::ExecutionContext;
use crate::interpreter::value::Value;
use crate::interpreter::function_pointer::FunctionArgs;
use crate::compiler::compiler::NativeFunction;
use crate::interpreter::array::Array;

pub fn load_functions(functions: &mut Vec<NativeFunction>) {
    functions.push(NativeFunction::new_rs(vec!["array".to_string()], String::from("new"), array_new));
    functions.push(NativeFunction::new_rs(vec!["array".to_string()], String::from("push"), array_push));
    functions.push(NativeFunction::new_rs(vec!["array".to_string()], String::from("get"), array_get));
    functions.push(NativeFunction::new_rs(vec!["array".to_string()], String::from("len"), array_len));
}

fn array_new(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let mut size = 0;
    let mut fill = Value::from_i64(0);

    if args.len() > 0 {
        size = args.get_next_i64(context);
    }

    if args.len() > 0 {
        fill = args.get_next(context);
    }

    let mut array = Array::new();
    for _ in 0..size {
        array.push(fill.clone());
    }

    Value::from_array(array)
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