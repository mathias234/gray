use crate::compiler::compiler::NativeFunction;
use crate::interpreter::function_pointer::FunctionArgs;
use crate::interpreter::interpreter::ExecutionContext;
use crate::interpreter::object::Object;
use crate::interpreter::value::{Value, DataValue};
use std::rc::Rc;
use std::time::Instant;
use std::mem::size_of;
use crate::interpreter::array::Array;

pub fn load_functions(functions: &mut Vec<NativeFunction>) {
    functions.push(NativeFunction::new_rs(
        vec!["debug".to_string()],
        String::from("start_watch"),
        start_watch,
    ));
    functions.push(NativeFunction::new_rs(
        vec!["debug".to_string()],
        String::from("stop_watch"),
        stop_watch,
    ));
    functions.push(NativeFunction::new_rs(
        vec!["debug".to_string()],
        String::from("size_of"),
        debug_size_of,
    ));
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
            &Value::from_i64(file.elapsed().as_nanos() as i64),
        );

        time_result.declare(
            Rc::from("millis".to_string()),
            &Value::from_i64(file.elapsed().as_millis() as i64),
        );

        time_result.declare(
            Rc::from("secs".to_string()),
            &Value::from_i64(file.elapsed().as_secs() as i64),
        );

        Value::from_object(time_result)
    } else {
        context.throw_error("Expected pointer to a watch object");
        Value::from_i64(-1)
    }
}

fn debug_size_of(context: &ExecutionContext, mut args: FunctionArgs) -> Value {
    let value = args.get_next(context);
    Value::from_i64(size_of_value(&value) as i64)
}

fn size_of_object(object: &Object) -> usize {
    let mut total = size_of::<Value>();

    for v in object.variables.borrow().iter() {
        total += size_of_value(v.1)
    }

    total
}

fn size_of_array(array: &Array) -> usize {
    let mut total = size_of::<Value>();

    for i in 0..array.len() {
        let v = array.get(i);
        total += size_of_value(&v)
    }

    total
}

fn size_of_value(value: &Value) -> usize {
    let size = match value.get_data_value() {
        DataValue::Object(obj) => size_of_object(obj),
        DataValue::Array(arr) => size_of_array(arr),

        DataValue::I64(_) => size_of::<Value>(),
        DataValue::F64(_) => size_of::<Value>(),
        DataValue::String(_) => size_of::<Value>(),
        DataValue::Pointer(_) => size_of::<Value>(),
        DataValue::FunctionPointer(_) => size_of::<Value>(),
        DataValue::Range(_) => size_of::<Value>(),
        DataValue::Iterator(_) => size_of::<Value>(),
        DataValue::Undefined => size_of::<Value>(),
    };

    size
}
