use gray::interpreter::interpreter::Interpreter;
use gray::interpreter::value::{Value, DataValue};
use std::rc::Rc;
use crate::declare_functions;

pub fn load_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(vec!["interp"], String::from("run_string"), run_string);
}

pub fn run_string(args: Vec<Value>) -> Value {
    let str = match args[0].get_data_value() {
        DataValue::String(str) => {
            str.clone()
        },
        _ => panic!("interp::run_string expected a string value"),
    };

    let mut interp_result = gray::load_string(&str);

    let mut interp;
    match interp_result {
        Ok(i) => interp = i,
        Err(e) => return Value::from_string(Rc::from(format!("{:?}", e).to_string()))
    }

    declare_functions(&mut interp);

    interp.run(None)
}