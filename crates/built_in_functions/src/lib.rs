use gray::interpreter::interpreter::Interpreter;
use gray::interpreter::value::{Value, DataValue};
use std::rc::Rc;

pub fn declare_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(String::from("print"), print_function);
    interpreter.set_native_function(String::from("format"), format_to_value);
    interpreter.set_native_function(String::from("array_push"), array_push);
    interpreter.set_native_function(String::from("array_get"), array_get);
    interpreter.set_native_function(String::from("array_len"), array_len);
}

fn array_push(args: Vec<Value>) -> Value {
    let mut array = args[0].clone();
    let pushed = 0;
    match array.get_data_value_mut() {
        DataValue::Array(array) => {
            for a in 1..args.len() {
                array.push(args[a].clone());
            }
        }
        _ => panic!("array_push() expects first parameter to be an array")
    }

    Value::from_i64(pushed)
}

fn array_get(args: Vec<Value>) -> Value {
    match &args[0].get_data_value() {
        DataValue::Array(array) => {
            match args[1].get_data_value() {
                DataValue::I64(i) => array.get(*i as usize),
                a => panic!("array_get() expects second parameter to be an integer was {:?}", a)
            }
        }
        _ => panic!("array_get() expects first parameter to be an array")
    }
}

fn array_len(args: Vec<Value>) -> Value {
    match &args[0].get_data_value() {
        DataValue::Array(array) => {
            Value::from_i64(array.len() as i64)
        }
        _ => panic!("array_get() expects first parameter to be an array")
    }
}

fn print_function(args: Vec<Value>) -> Value {
    println!("{}", value_to_string(&format_to_value(args)));

    Value::from_i64(0)
}

fn format_to_value(args: Vec<Value>) -> Value {
    let format_str = &args[0];
    let format_str = match format_str.get_data_value() {
        DataValue::String(str) => str,
        _ => panic!("Only strings can be used as format string"),
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
    }
}