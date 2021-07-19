use gray::interpreter::interpreter::ExecutionContext;
use gray::interpreter::value::Value;
use std::rc::Rc;
use crate::declare_functions;
use gray::interpreter::function_pointer::FunctionArgs;
use gray::compiler::compiler::NativeFunction;

pub fn load_functions(functions: &mut Vec<NativeFunction>) {
    functions.push(NativeFunction::new(vec!["interp".to_string()], String::from("run_string"), run_string));
}

pub fn run_string(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let str = args.get_next_string(context);

    let functions = declare_functions();


    let interp_result = gray::load_string(&str, functions);

    let mut interp;
    match interp_result {
        Ok(i) => interp = i,
        Err(e) => return Value::from_string(Rc::from(format!("{:?}", e).to_string()))
    }


    interp.run(None)
}