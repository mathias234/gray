mod math;
mod array;
mod io;
mod gray_interp;

use gray::interpreter::interpreter::Interpreter;
use gray::interpreter::value::{Value, DataValue};
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
}

fn assert_eq(mut args: FunctionArgs) -> Value {
    let received_value = args.get_next().clone();
    let expected_value = args.get_next().clone();

    assert_eq!(received_value, expected_value);

    Value::from_i64(0)
}


fn print_function(args: FunctionArgs) -> Value {
    println!("{}", value_to_string(&format_to_value(args)));

    Value::from_i64(0)
}

fn format_to_value(mut args: FunctionArgs) -> Value {
    let format_str = args.get_next_string();

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

                let value_formatted = value_to_string(&args.get_next());
                formatted_string.push_str(&value_formatted);
            }
            c => {
                formatted_string.push(c);
            }
        }
    }

    Value::from_string(Rc::from(formatted_string))
}

fn value_to_string(args: &Value) -> String {
    match args.get_data_value() {
        DataValue::F64(float_value) => format!("{}", float_value),
        DataValue::I64(int_value) => format!("{}", int_value),
        DataValue::Object(object) => format!("{:?}", object),
        DataValue::String(string) => format!("{}", string),
        DataValue::Array(array) => format!("{}", array),
        DataValue::Pointer(_) => format!("Internal Pointer"),
    }
}