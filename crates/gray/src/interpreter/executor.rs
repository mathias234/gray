use crate::bytecode::instructions::instructions::CallArgs;
use crate::bytecode::instructions::instructions::Instruction;
use crate::bytecode::label::Label;
use crate::bytecode::register::Register;
use crate::interpreter::array::Array;
use crate::interpreter::interpreter::ExecutionContext;
use crate::interpreter::interpreter::VariableHandle;
use crate::interpreter::iterator::ArrayIterator;
use crate::interpreter::iterator::IteratorHolder;
use crate::interpreter::object::Object;
use crate::interpreter::value::{DataValue, Value};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt::Write;
use std::rc::Rc;

pub fn execute(instruction: &Instruction, context: &mut ExecutionContext) {
    match instruction {
        Instruction::NotAnInstruction => {}
        Instruction::LoadImmediate(v) => execute_load_immidiate(v, context),
        Instruction::LoadRegister(r) => execute_load_register(r, context),
        Instruction::LoadArgument(a) => execute_load_argument(*a, context),
        Instruction::ParamsList(s) => execute_params_list(*s, context),
        Instruction::Store(r) => execute_store(r, context),
        Instruction::Call(args) => execute_call(args, context),
        Instruction::Return => execute_return(context),
        Instruction::DeclareVariable(v) => execute_declare_variable(*v, context),
        Instruction::SetVariable(v) => execute_set_variable(*v, context),
        Instruction::GetVariable(v) => execute_get_variable(*v, context),
        Instruction::PushScope => context.push_scope(),
        Instruction::PopScope => context.pop_scope(),
        Instruction::PushBreakContinueScope(break_label, continue_label) => {
            context.push_break_continue_scope(*break_label, *continue_label)
        }
        Instruction::PopBreakContinueScope => {
            context.pop_break_continue_scope();
        }
        Instruction::Break => context.set_break(),
        Instruction::Continue => context.set_continue(),
        Instruction::Range(rhs) => execute_range(rhs, context),
        Instruction::CreateIterator => execute_create_iterator(context),
        Instruction::IteratorGetNext => execute_iterator_get_next(context),
        Instruction::IteratorEmpty => execute_iterator_empty(context),
        Instruction::NegateValue => execute_negate_value(context),
        Instruction::CompareEq(register) => execute_compare_eq(register, context),
        Instruction::CompareNotEq(register) => execute_compare_neq(register, context),
        Instruction::CompareGreaterThan(register) => execute_compare_gt(register, context),
        Instruction::CompareLessThan(register) => execute_compare_lt(register, context),
        Instruction::CompareLessThanOrEqual(register) => execute_compare_lte(register, context),
        Instruction::CompareGreaterThanOrEqual(register) => execute_compare_gte(register, context),
        Instruction::And(register) => execute_compare_and(register, context),
        Instruction::Or(register) => execute_compare_or(register, context),
        Instruction::Jump(label) => execute_jump(label, context),
        Instruction::JumpZero(label) => execute_jump_zero(label, context),
        Instruction::JumpNotZero(label) => execute_jump_not_zero(label, context),
        Instruction::Add(register) => execute_add(register, context),
        Instruction::Subtract(register) => execute_subtract(register, context),
        Instruction::Multiply(register) => execute_multiply(register, context),
        Instruction::Divide(register) => execute_divide(register, context),
        Instruction::CreateEmptyObject => execute_create_empty_obj(context),
        Instruction::SetObjectMember(obj, acc) => execute_set_obj_member(obj, acc, context),
        Instruction::GetObjectMember(obj, acc) => execute_get_obj_member(obj, acc, context),
        Instruction::CreateEmptyArray => execute_create_empty_array(context),
        Instruction::PushArray(array) => execute_push_array(array, context),
        Instruction::GetArrayLength => execute_get_array_length(context),
        Instruction::GetArray(array) => execute_get_array(array, context),
        Instruction::SetArray(array, value) => execute_set_array(array, value, context),
    }
}

fn execute_load_immidiate(value: &Value, context: &mut ExecutionContext) {
    context.set_accumulator(value.clone());
}

fn execute_load_register(register: &Register, context: &mut ExecutionContext) {
    context.set_accumulator(context.get_register(register));
}

fn execute_load_argument(argument: usize, context: &mut ExecutionContext) {
    context.set_accumulator(context.get_argument(argument));
}

fn execute_params_list(start: usize, context: &mut ExecutionContext) {
    let mut array = Array::new();

    for arg in start..context.get_argument_count() {
        array.push(context.get_argument(arg));
    }

    context.set_accumulator(Value::from_array(array));
}

fn execute_store(register: &Register, context: &mut ExecutionContext) {
    let accumulator = context.get_accumulator();
    context.set_register(register, accumulator);
}

fn execute_call(call_args: &CallArgs, context: &mut ExecutionContext) {
    if call_args.arguments.len() > 0 {
        let first_arg = context.get_register(&call_args.arguments[0]);
        if let Some(struct_def) = first_arg.as_struct_def() {
            context.set_register(
                &call_args.arguments[0],
                Value::from_object(struct_def.create_object()),
            );
        }
    }

    if call_args.pass_through_args {
        context.set_call_arguments(context.get_arguments())
    } else {
        let mut args = Vec::new();

        for arg in &call_args.arguments {
            args.push(context.get_register(&arg));
        }

        context.set_call_arguments(args);
    }
    context.set_call(call_args.block_id);
}

fn execute_return(context: &mut ExecutionContext) {
    context.set_return();
}

fn execute_declare_variable(variable: VariableHandle, context: &mut ExecutionContext) {
    let value = context.get_accumulator();
    context.declare_variable(variable, &value);
}

fn execute_set_variable(variable: VariableHandle, context: &mut ExecutionContext) {
    let value = context.get_accumulator();
    context.set_variable(variable, &value);
}

fn execute_get_variable(variable: VariableHandle, context: &mut ExecutionContext) {
    let value = context.get_variable(variable);
    context.set_accumulator(value);
}

fn execute_range(rhs_register: &Register, context: &mut ExecutionContext) {
    let lhs_value = context.get_accumulator();
    let rhs_value = context.get_register(rhs_register);

    let lhs_integer = match lhs_value.get_data_value() {
        DataValue::I64(v) => *v,
        _ => {
            context.throw_error("Expected integer");
            return;
        }
    };

    let rhs_integer = match rhs_value.get_data_value() {
        DataValue::I64(v) => *v,
        _ => {
            context.throw_error("Expected integer");
            return;
        }
    };

    if lhs_integer > rhs_integer {
        context.throw_error("Expected end to be larger then start");
        return;
    }

    context.set_accumulator(Value::from_range(lhs_integer, rhs_integer));
}

fn execute_create_iterator(context: &mut ExecutionContext) {
    let value = context.get_accumulator();

    let iterator = match &value.get_data_value() {
        DataValue::Range(range) => IteratorHolder {
            iterator: Rc::from(RefCell::from(range.clone())),
        },
        DataValue::Array(array) => {
            let array_iter = ArrayIterator {
                array: array.clone(),
                index: 0,
            };

            IteratorHolder {
                iterator: Rc::from(RefCell::from(array_iter)),
            }
        }

        value => {
            context.throw_error(&format!("Could not convert {:?} to iterator", value));
            return;
        }
    };

    context.set_accumulator(Value::from_iterator(iterator));
}

fn execute_iterator_get_next(context: &mut ExecutionContext) {
    let mut iterator = context.get_accumulator();
    let iterator = &mut iterator.get_data_value_mut();
    let iterator = match iterator {
        DataValue::Iterator(iterator) => iterator,
        _ => {
            context.throw_error("Expected an iterator");
            return;
        }
    };

    if let Some(value) = iterator.iterator.borrow_mut().pop_next() {
        context.set_accumulator(value);
        return;
    }

    context.set_accumulator(Value::from_undefined());
}

fn execute_iterator_empty(context: &mut ExecutionContext) {
    let mut iterator = context.get_accumulator();
    let iterator = &mut iterator.get_data_value_mut();
    let iterator = match iterator {
        DataValue::Iterator(iterator) => iterator,
        _ => {
            context.throw_error("Expected an iterator");
            return;
        }
    };

    if iterator.iterator.borrow_mut().empty() {
        context.set_accumulator(Value::from_i64(1));
        return;
    }

    context.set_accumulator(Value::from_i64(0));
}

fn execute_negate_value(context: &mut ExecutionContext) {
    let value = context.get_accumulator();

    let result = match value.get_data_value() {
        DataValue::I64(v) => Value::from_i64(v * -1),
        DataValue::F64(v) => Value::from_f64(v * -1.0),
        v => {
            context.throw_error(&format!("Unable to negate value {:?}", v));
            return;
        }
    };

    context.set_accumulator(result)
}

fn execute_compare_eq(register: &Register, context: &mut ExecutionContext) {
    let lhs = context.get_accumulator();
    let rhs = context.get_register(register);

    context.set_accumulator(Value::from_i64((lhs.eq(&rhs)) as i64));
}

fn execute_compare_neq(register: &Register, context: &mut ExecutionContext) {
    let lhs = context.get_accumulator();
    let rhs = context.get_register(register);

    context.set_accumulator(Value::from_i64((!lhs.eq(&rhs)) as i64));
}

fn execute_compare_lt(register: &Register, context: &mut ExecutionContext) {
    let lhs = context.get_accumulator();
    let rhs = context.get_register(register);

    let result = (lhs.partial_cmp(context, &rhs).unwrap() == Ordering::Less) as i64;
    context.set_accumulator(Value::from_i64(result));
}

fn execute_compare_gt(register: &Register, context: &mut ExecutionContext) {
    let lhs = context.get_accumulator();
    let rhs = context.get_register(register);

    let result = (lhs.partial_cmp(context, &rhs).unwrap() == Ordering::Greater) as i64;
    context.set_accumulator(Value::from_i64(result));
}

fn execute_compare_lte(register: &Register, context: &mut ExecutionContext) {
    let lhs = context.get_accumulator();
    let rhs = context.get_register(register);

    let result = (lhs.partial_cmp(context, &rhs).unwrap() != Ordering::Greater) as i64;
    context.set_accumulator(Value::from_i64(result));
}

fn execute_compare_gte(register: &Register, context: &mut ExecutionContext) {
    let lhs = context.get_accumulator();
    let rhs = context.get_register(register);

    let result = (lhs.partial_cmp(context, &rhs).unwrap() != Ordering::Less) as i64;
    context.set_accumulator(Value::from_i64(result));
}

fn execute_compare_and(register: &Register, context: &mut ExecutionContext) {
    let lhs = context.get_accumulator();
    let rhs = context.get_register(register);

    let true_value = Value::from_i64(1);
    if lhs.eq(&true_value) && rhs.eq(&true_value) {
        context.set_accumulator(true_value);
        return;
    }

    context.set_accumulator(Value::from_i64(0));
}

fn execute_compare_or(register: &Register, context: &mut ExecutionContext) {
    let lhs = context.get_accumulator();
    let rhs = context.get_register(register);

    let true_value = Value::from_i64(1);
    if lhs.eq(&true_value) || rhs.eq(&true_value) {
        context.set_accumulator(true_value);
        return;
    }

    context.set_accumulator(Value::from_i64(0));
}

fn execute_jump(target: &Label, context: &mut ExecutionContext) {
    context.set_jump_target(target)
}

fn execute_jump_zero(target: &Label, context: &mut ExecutionContext) {
    if context.get_accumulator().eq(&Value::from_i64(0)) {
        context.set_jump_target(target)
    }
}

fn execute_jump_not_zero(target: &Label, context: &mut ExecutionContext) {
    if !context.get_accumulator().eq(&Value::from_i64(0)) {
        context.set_jump_target(target)
    }
}

fn execute_add(register: &Register, context: &mut ExecutionContext) {
    let value = context.get_accumulator();
    let value2 = context.get_register(register);
    let result = value.add(context, value2);
    context.set_accumulator(result);
}

fn execute_subtract(register: &Register, context: &mut ExecutionContext) {
    let value = context.get_accumulator();
    let value2 = context.get_register(register);
    let result = value.sub(context, value2);
    context.set_accumulator(result);
}

fn execute_multiply(register: &Register, context: &mut ExecutionContext) {
    let value = context.get_accumulator();
    let value2 = context.get_register(register);
    let result = value.mul(context, value2);
    context.set_accumulator(result);
}

fn execute_divide(register: &Register, context: &mut ExecutionContext) {
    let value = context.get_accumulator();
    let value2 = context.get_register(register);
    let result = value.div(context, value2);
    context.set_accumulator(result);
}

fn execute_create_empty_obj(context: &mut ExecutionContext) {
    context.set_accumulator(Value::from_object(Object::new()));
}

fn execute_set_obj_member(object: &Register, accessor: &Register, context: &mut ExecutionContext) {
    let mut obj = match context.get_register(object).get_data_value() {
        DataValue::Object(object) => object.clone(),
        _ => {
            context.throw_error("Trying to set a member value of something that is not an object");
            return;
        }
    };

    let accessor_value = match context.get_register(accessor).get_data_value() {
        DataValue::String(name) => name.clone(),
        _ => {
            context.throw_error("Expected object accessor to be an identifier");
            return;
        }
    };

    obj.declare(accessor_value, &context.get_accumulator());

    context.set_register(object, Value::from_object(obj));
}

fn execute_get_obj_member(object: &Register, accessor: &Register, context: &mut ExecutionContext) {
    let obj = match context.get_register(object).get_data_value() {
        DataValue::Object(object) => object.clone(),
        DataValue::StructDef(def) => def.create_object(),
        value => {
            context.throw_error(&format!(
                "Trying to get a member value of something that is not an object {:?}",
                value
            ));
            return;
        }
    };

    let accessor_value = match context.get_register(accessor).get_data_value() {
        DataValue::String(name) => name.clone(),
        _ => {
            context.throw_error("Expected object accessor to be an identifier");
            return;
        }
    };

    let value = if let Some(value) = obj.get(accessor_value) {
        value
    } else {
        context.throw_error("Could not find member on object");
        return;
    };

    context.set_accumulator(value);
}

fn execute_create_empty_array(context: &mut ExecutionContext) {
    context.set_accumulator(Value::from_array(Array::new()));
}

fn execute_push_array(array: &Register, context: &mut ExecutionContext) {
    let value = context.get_accumulator();
    let mut array = context.get_register(array);
    match array.get_data_value_mut() {
        DataValue::Array(a) => {
            a.push(value);
        }
        _ => {
            context.throw_error("Error pushing to value that is not an array");
            return;
        }
    }
}

fn execute_get_array_length(context: &mut ExecutionContext) {
    let array = context.get_accumulator();

    let len = match &array.get_data_value() {
        DataValue::Array(array) => array.len() as i64,
        _ => {
            context.throw_error("Expected array");
            -1
        }
    };

    context.set_accumulator(Value::from_i64(len));
}

fn execute_get_array(array: &Register, context: &mut ExecutionContext) {
    let index = context.get_accumulator();
    let mut array = context.get_register(array);

    //println!("Get Array variables is\n\tArray {:?}\n\tIndex {:?}", array, index);

    let index = match index.get_data_value() {
        DataValue::I64(v) => *v as usize,
        d => {
            context.throw_error(&format!(
                "Array can only be indexed with an integer {:?}",
                d
            ));
            return;
        }
    };

    match array.get_data_value_mut() {
        DataValue::Array(a) => {
            context.set_accumulator(a.get(index));
        }
        v => {
            context.throw_error(&format!(
                "Error getting value from object that is not an array {:?}",
                v
            ));
            return;
        }
    }
}

fn execute_set_array(array: &Register, value: &Register, context: &mut ExecutionContext) {
    let index = context.get_accumulator();
    let mut array = context.get_register(array);

    //println!("Set Array variables is\n\tArray {:?}\n\tIndex {:?}\n\tValue {:?}", array, index, context.get_accumulator());

    let index = match index.get_data_value() {
        DataValue::I64(v) => *v as usize,
        d => {
            context.throw_error(&format!(
                "Array can only be indexed with an integer {:?}",
                d
            ));
            return;
        }
    };

    match array.get_data_value_mut() {
        DataValue::Array(a) => {
            a.set(index, context.get_register(value));
        }
        _ => {
            context.throw_error("Error setting value to object that is not an array");
            return;
        }
    }
}

pub fn to_string(instruction: &Instruction) -> String {
    match instruction {
        Instruction::NotAnInstruction => "Not An Instruction".to_string(),
        Instruction::LoadImmediate(v) => format!("LoadImmediate value: {}", v),
        Instruction::LoadRegister(r) => format!("LoadRegister value: {}", r),
        Instruction::LoadArgument(a) => format!("LoadArgument argument: {}", a),
        Instruction::ParamsList(s) => format!("ParamsList argument: {}", s),
        Instruction::Store(r) => format!("Store {}", r),
        Instruction::Call(args) => to_string_call(args),
        Instruction::Return => "Return".to_string(),
        Instruction::DeclareVariable(v) => format!("DeclareVariable: {}", v),
        Instruction::SetVariable(v) => format!("SetVariable: {}", v),
        Instruction::GetVariable(v) => format!("GetVariable: {}", v),
        Instruction::PushScope => "PushScope".to_string(),
        Instruction::PopScope => "PopScope".to_string(),
        Instruction::PushBreakContinueScope(b_label, c_label) => {
            format!("PushBreakContinueScope {} {}", b_label, c_label)
        }
        Instruction::PopBreakContinueScope => "PopBreakContinueScope".to_string(),
        Instruction::Break => "Break".to_string(),
        Instruction::Continue => "Continue".to_string(),
        Instruction::Range(rhs) => format!("Range {}", rhs),
        Instruction::CreateIterator => "CreateIterator".to_string(),
        Instruction::IteratorGetNext => "IteratorGetNext".to_string(),
        Instruction::IteratorEmpty => "IteratorEmpty".to_string(),
        Instruction::NegateValue => "NegateValue".to_string(),
        Instruction::CompareEq(r) => format!("CompareEq {}", r),
        Instruction::CompareNotEq(r) => format!("CompareNotEq {}", r),
        Instruction::CompareGreaterThan(r) => format!("CompareGreaterThan {}", r),
        Instruction::CompareLessThan(r) => format!("CompareLessThan {}", r),
        Instruction::CompareLessThanOrEqual(r) => format!("CompareLessThanOrEqual {}", r),
        Instruction::CompareGreaterThanOrEqual(r) => format!("CompareGreaterThanOrEqual {}", r),
        Instruction::And(r) => format!("And {}", r),
        Instruction::Or(r) => format!("Or {}", r),
        Instruction::Jump(l) => format!("Jump {}", l),
        Instruction::JumpZero(l) => format!("JumpZero {}", l),
        Instruction::JumpNotZero(l) => format!("JumpNotZero {}", l),
        Instruction::Add(r) => format!("Add {}", r),
        Instruction::Subtract(r) => format!("Subtract {}", r),
        Instruction::Multiply(r) => format!("Multiply {}", r),
        Instruction::Divide(r) => format!("Divide {}", r),
        Instruction::CreateEmptyObject => "CreateEmptyObject".to_string(),
        Instruction::SetObjectMember(o, a) => format!("SetObjectMember {} {}", o, a),
        Instruction::GetObjectMember(o, a) => format!("GetObjectMember {} {}", o, a),
        Instruction::CreateEmptyArray => "CreateEmptyArray".to_string(),
        Instruction::PushArray(a) => format!("PushArray {}", a),
        Instruction::GetArrayLength => "GetArrayLength".to_string(),
        Instruction::GetArray(a) => format!("GetArray {}", a),
        Instruction::SetArray(a, v) => format!("SetArray {} {}", a, v),
    }
}

fn to_string_call(args: &CallArgs) -> String {
    let mut writer = String::new();

    if args.pass_through_args {
        write!(
            &mut writer,
            "Call block {} Args: pass through",
            args.block_id
        )
        .unwrap();
    } else {
        write!(&mut writer, "Call block {} Args: (", args.block_id).unwrap();
        let mut idx = 0;
        for a in &args.arguments {
            write!(&mut writer, "{}", a).unwrap();

            if idx < args.arguments.len() - 1 {
                write!(&mut writer, ", ").unwrap();
            }

            idx += 1;
        }

        write!(&mut writer, ")").unwrap();
    }
    writer
}
