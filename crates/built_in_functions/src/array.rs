use gray::interpreter::interpreter::Interpreter;
use gray::interpreter::value::{Value, DataValue};

pub fn load_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(vec!["array"], String::from("push"), array_push);
    interpreter.set_native_function(vec!["array"], String::from("get"), array_get);
    interpreter.set_native_function(vec!["array"], String::from("len"), array_len);
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