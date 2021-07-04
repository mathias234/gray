use gray::interpreter::interpreter::Interpreter;
use gray::interpreter::value::{Value, DataValue};
use gray::interpreter::function_pointer::FunctionArgs;

pub fn load_functions(interpreter: &mut Interpreter) {
    interpreter.set_native_function(vec!["math"], String::from("abs"), math_abs);
    interpreter.set_native_function(vec!["math"], String::from("sin"), math_sin);
    interpreter.set_native_function(vec!["math"], String::from("cos"), math_cos);
}

pub fn math_abs(mut args: FunctionArgs) -> Value {
    let result = match args.get_next().get_data_value() {
        DataValue::F64(value) => Value::from_f64(f64::abs(*value)),
        DataValue::I64(value) => Value::from_i64(i64::abs(*value)),
        d => panic!("math::abs() expects argument to be either float or integer was {:?}", d),
    };

    result
}

pub fn math_sin(mut args: FunctionArgs) -> Value {
    let result = match args.get_next().get_data_value() {
        DataValue::F64(value) => Value::from_f64(f64::sin(*value)),
        DataValue::I64(value) => Value::from_f64(f64::sin(*value as f64)),
        d => panic!("math::sin() expects argument to be either float or integer was {:?}", d),
    };

    result
}

pub fn math_cos(mut args: FunctionArgs) -> Value {
    let result = match args.get_next().get_data_value() {
        DataValue::F64(value) => Value::from_f64(f64::cos(*value)),
        DataValue::I64(value) => Value::from_f64(f64::cos(*value as f64)),
        d => panic!("math::cos() expects argument to be either float or integer was {:?}", d),
    };

    result
}