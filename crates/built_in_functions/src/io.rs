use gray::interpreter::interpreter::Interpreter;
use gray::interpreter::value::{Value, DataValue};
use std::rc::Rc;
use std::io::Read;

pub fn load_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(vec!["fs"], String::from("open"), fs_open);
    interpreter.set_native_function(vec!["fs"], String::from("read_to_string"), fs_read_to_string);
}

pub fn fs_open(args: Vec<Value>) -> Value {
    let file_name = match args[0].get_data_value() {
        DataValue::String(str) => str.clone(),
        _ => panic!("Expected filename to be string"),
    };

    let file = std::fs::File::open(file_name.as_str()).expect("Failed to open file");

    Value::to_pointer(file)
}

pub fn fs_read_to_string(args: Vec<Value>) -> Value {
    let mut file = value_to_file(&args[0]);

    let mut buffer = String::new();
    file.read_to_string(&mut buffer).expect("Error reading files contents");

    Value::from_string(Rc::from(buffer))
}

fn value_to_file(value: &Value) -> std::fs::File {
    match value.get_data_value() {
        DataValue::Pointer(p) => {
            let p = p.borrow();
            if let Some(file) = p.downcast_ref::<std::fs::File>() {
                file.try_clone().unwrap()
            } else {
                panic!("Expected pointer to a file from fs::open");
            }
        }
        _ => panic!("Expected pointer to a file from fs::open"),
    }
}