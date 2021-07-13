use gray::interpreter::interpreter::{Interpreter, ExecutionContext};
use gray::interpreter::function_pointer::FunctionArgs;
use gray::interpreter::value::Value;
use std::time::Instant;

pub fn load_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(vec!["debug"], String::from("start_watch"), start_watch);
    interpreter.set_native_function(vec!["debug"], String::from("stop_watch"), stop_watch);
}

fn start_watch(_: &ExecutionContext, _: FunctionArgs) -> Value {
    let instant = Instant::now();
    Value::from_any(instant)
}

fn stop_watch(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let pointer = args.get_next_pointer(context);
    let p = pointer.borrow();
    if let Some(file) = p.downcast_ref::<Instant>() {
        Value::from_i64(file.elapsed().as_millis() as i64)
    } else {
        context.throw_error("Expected pointer to a watch object");
        Value::from_i64(-1)
    }
}


