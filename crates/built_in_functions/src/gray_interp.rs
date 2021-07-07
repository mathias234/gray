use gray::interpreter::interpreter::{Interpreter, ExecutionContext};
use gray::interpreter::value::Value;
use std::rc::Rc;
use crate::declare_functions;
use gray::interpreter::function_pointer::FunctionArgs;

pub fn load_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(vec!["interp"], String::from("run_string"), run_string);
}

pub fn run_string(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let str = args.get_next_string(context);

    let interp_result = gray::load_string(&str);

    let mut interp;
    match interp_result {
        Ok(i) => interp = i,
        Err(e) => return Value::from_string(Rc::from(format!("{:?}", e).to_string()))
    }

    declare_functions(&mut interp);

    interp.run(None)
}