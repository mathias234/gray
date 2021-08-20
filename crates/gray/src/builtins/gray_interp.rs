use crate::builtins::declare_functions;
use crate::compiler::compiler::NativeFunction;
use crate::interpreter::function_pointer::FunctionArgs;
use crate::interpreter::interpreter::ExecutionContext;
use crate::interpreter::value::Value;
use crate::load_string;
use std::rc::Rc;

pub fn load_functions(functions: &mut Vec<NativeFunction>) {
    functions.push(NativeFunction::new_rs(
        vec!["interp".to_string()],
        String::from("run_string"),
        run_string,
    ));
}

pub fn run_string(context: &mut ExecutionContext, mut args: FunctionArgs) -> Value {
    let str = args.get_next_string(context);

    let functions = declare_functions();

    let interp_result = load_string(&str, functions);

    let mut interp;
    match interp_result {
        Ok(i) => interp = i,
        Err(e) => return Value::from_string(Rc::from(format!("{:?}", e).to_string())),
    }

    interp.run(None)
}
