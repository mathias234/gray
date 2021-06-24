use gray::interpreter::interpreter::Interpreter;
use gray::interpreter::value::{Value, DataValue};

pub fn declare_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(String::from("print"), print_function);
    interpreter.set_native_function(String::from("format"), format_to_value);
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

    Value::from_string(formatted_string)
}

fn value_to_string(args: &Value) -> String {
    match args.get_data_value() {
        DataValue::F64(float_value) => format!("{}", float_value),
        DataValue::I64(int_value) => format!("{}", int_value),
        DataValue::Object(object) => format!("{:?}", object),
        DataValue::String(string) => format!("{}", string),
    }
}