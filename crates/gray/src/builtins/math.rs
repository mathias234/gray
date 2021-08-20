use crate::compiler::compiler::NativeFunction;
use crate::interpreter::function_pointer::FunctionArgs;
use crate::interpreter::interpreter::ExecutionContext;
use crate::interpreter::value::{DataValue, Value};

pub fn load_functions(functions: &mut Vec<NativeFunction>) {
    functions.push(NativeFunction::new_rs(
        vec!["math".to_string()],
        "abs".to_string(),
        math_abs,
    ));
    functions.push(NativeFunction::new_rs(
        vec!["math".to_string()],
        "sin".to_string(),
        math_sin,
    ));
    functions.push(NativeFunction::new_rs(
        vec!["math".to_string()],
        "cos".to_string(),
        math_cos,
    ));
}

pub fn math_abs(context: &mut ExecutionContext, mut args: FunctionArgs) -> Value {
    let result = match args.get_next(context).get_data_value() {
        DataValue::F64(value) => Value::from_f64(f64::abs(*value)),
        DataValue::I64(value) => Value::from_i64(i64::abs(*value)),
        d => context.throw_error(&format!(
            "Expects one argument that is either float or integer was {:?}",
            d
        )),
    };

    result
}

pub fn math_sin(context: &mut ExecutionContext, mut args: FunctionArgs) -> Value {
    let result = match args.get_next(context).get_data_value() {
        DataValue::F64(value) => Value::from_f64(f64::sin(*value)),
        DataValue::I64(value) => Value::from_f64(f64::sin(*value as f64)),
        d => context.throw_error(&format!(
            "Expects one argument that is either float or integer was {:?}",
            d
        )),
    };

    result
}

pub fn math_cos(context: &mut ExecutionContext, mut args: FunctionArgs) -> Value {
    let result = match args.get_next(context).get_data_value() {
        DataValue::F64(value) => Value::from_f64(f64::cos(*value)),
        DataValue::I64(value) => Value::from_f64(f64::cos(*value as f64)),
        d => context.throw_error(&format!(
            "Expects one argument that is either float or integer was {:?}",
            d
        )),
    };

    result
}
