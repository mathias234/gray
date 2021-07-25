mod math;
mod array;
mod io;
mod gray_interp;
mod debug;

use crate::interpreter::interpreter::ExecutionContext;
use crate::interpreter::value::{Value};
use std::rc::Rc;
use crate::interpreter::function_pointer::FunctionArgs;
use crate::compiler::compiler::NativeFunction;

pub fn declare_functions() -> Vec<NativeFunction> {
    let mut functions = Vec::new();
    functions.push(NativeFunction::new_rs(Vec::new(), String::from("print"), print_function));
    functions.push(NativeFunction::new_rs(Vec::new(), String::from("println"), println_function));
    functions.push(NativeFunction::new_rs(Vec::new(), String::from("format"), format_to_value));
    functions.push(NativeFunction::new_rs(Vec::new(), String::from("assert_eq"), assert_eq));

     math::load_functions(&mut functions);
     array::load_functions(&mut functions);
     io::load_functions(&mut functions);
     gray_interp::load_functions(&mut functions);
     debug::load_functions(&mut functions);

    functions
}

fn assert_eq(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let received_value = args.get_next(context).clone();
    let expected_value = args.get_next(context).clone();

    if !received_value.eq(context, &expected_value) {
        return context.throw_error(&format!("Assertion failed {:?} == {:?}", received_value.get_data_value(), expected_value.get_data_value()));
    }
    
    Value::from_i64(0)
}


fn println_function(context: &ExecutionContext, args: FunctionArgs) -> Value {
    print_function(context, args);
    println!();

    Value::from_i64(0)
}

fn print_function(context: &ExecutionContext, args: FunctionArgs) -> Value {
    print!("{}", format_to_value(context, args).to_string());

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

