mod math;
mod array;
mod io;
mod gray_interp;
mod debug;

use gray::interpreter::interpreter::{Interpreter, ExecutionContext};
use gray::interpreter::value::{Value};
use std::rc::Rc;
use gray::interpreter::function_pointer::FunctionArgs;

pub fn declare_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(Vec::new(), String::from("print"), print_function);
    interpreter.set_native_function(Vec::new(), String::from("format"), format_to_value);
    interpreter.set_native_function(Vec::new(), String::from("assert_eq"), assert_eq);

    math::load_functions(interpreter);
    array::load_functions(interpreter);
    io::load_functions(interpreter);
    gray_interp::load_functions(interpreter);
    debug::load_functions(interpreter);
}

fn assert_eq(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let received_value = args.get_next(context).clone();
    let expected_value = args.get_next(context).clone();

    if !received_value.eq(context, &expected_value) {
        return context.throw_error(&format!("Assertion failed {:?} == {:?}", received_value.get_data_value(), expected_value.get_data_value()));
    }
    
    Value::from_i64(0)
}


fn print_function(context: &ExecutionContext, args: FunctionArgs) -> Value {
    println!("{}", format_to_value(context, args).to_string());

    Value::from_i64(0)
}

fn format_to_value(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let format_str = args.get_next_string(context);

    let mut formatted_string = String::new();

    let mut chars = format_str.chars();
    loop {
        let char = chars.next();
        if char.is_none() {
            break;
        }

        match char.unwrap() {
            '{' => {
                chars.next();

                let value_formatted = args.get_next(context).to_string();
                formatted_string.push_str(&value_formatted);
            }
            c => {
                formatted_string.push(c);
            }
        }
    }

    Value::from_string(Rc::from(formatted_string))
}

