use gray::interpreter::interpreter::ExecutionContext;
use gray::interpreter::function_pointer::FunctionArgs;
use gray::interpreter::value::Value;
use std::time::Instant;
use gray::interpreter::object::Object;
use std::rc::Rc;
use gray::compiler::compiler::NativeFunction;

pub fn load_functions(functions: &mut Vec<NativeFunction>) {
    functions.push(NativeFunction::new(vec!["debug".to_string()], String::from("start_watch"), start_watch));
    functions.push(NativeFunction::new(vec!["debug".to_string()], String::from("stop_watch"), stop_watch));
}

fn start_watch(_: &ExecutionContext, _: FunctionArgs) -> Value {
    let instant = Instant::now();
    Value::from_any(instant)
}

fn stop_watch(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let pointer = args.get_next_pointer(context);
    let p = pointer.borrow();
    if let Some(file) = p.downcast_ref::<Instant>() {
        let mut time_result = Object::new();
        time_result.declare(
            Rc::from("nanos".to_string()),
            &Value::from_i64(file.elapsed().as_nanos() as i64)
        );

        time_result.declare(
            Rc::from("millis".to_string()),
            &Value::from_i64(file.elapsed().as_millis() as i64)
        );

        time_result.declare(
            Rc::from("secs".to_string()),
            &Value::from_i64(file.elapsed().as_secs() as i64)
        );


        Value::from_object(time_result)
    } else {
        context.throw_error("Expected pointer to a watch object");
        Value::from_i64(-1)
    }
}


