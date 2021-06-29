mod math;
mod array;
mod io;
mod gray_interp;

use gray::interpreter::interpreter::Interpreter;
use gray::interpreter::value::{Value, DataValue};
use std::rc::Rc;

pub fn declare_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(Vec::new(), String::from("print"), print_function);
    interpreter.set_native_function(Vec::new(), String::from("format"), format_to_value);
    interpreter.set_native_function(Vec::new(), String::from("assert_eq"), assert_eq);

    math::load_functions(interpreter);
    array::load_functions(interpreter);
    io::load_functions(interpreter);
    gray_interp::load_functions(interpreter);
}

fn assert_eq(args: Vec<Value>) -> Value {
    let received_value = &args[0];
    let expected_value = &args[1];

    assert_eq!(received_value, expected_value);

    Value::from_i64(0)
}


fn print_function(args: Vec<Value>) -> Value {
    println!("{}", value_to_string(&format_to_value(args)));

    Value::from_i64(0)
}

fn format_to_value(args: Vec<Value>) -> Value {
    let format_str = &args[0];
    let format_str = match format_str.get_data_value() {
        DataValue::String(str) => str,
        d => panic!("Only strings can be used as format string tried to print {:?}", d),
    };

    let mut arg_idx = 1;

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

                let value_formatted = value_to_string(&args[arg_idx]);
                formatted_string.push_str(&value_formatted);

                arg_idx += 1;
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