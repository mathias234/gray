use gray::interpreter::value::Value;
use std::ffi::{CStr, c_void};
use std::rc::Rc;
use std::os::raw::c_char;
use gray::interpreter::interpreter::Interpreter;

unsafe fn c_str_to_string(str: *const c_char) -> String {
    let c_str = CStr::from_ptr(str);
    String::from(c_str.to_str().unwrap())
}

#[repr(C)]
pub struct ValuePointer {
    value: *mut c_void,
}

fn value_to_value_pointer(value: Value) -> ValuePointer {
    let boxed = Box::from(value);

    ValuePointer {
        value: Box::into_raw(boxed) as *mut c_void
    }
}

#[no_mangle]
pub unsafe extern "C" fn value_from_string(str: *const c_char) -> ValuePointer {
    value_to_value_pointer(Value::from_string(Rc::from(c_str_to_string(str))))
}

#[no_mangle]
pub unsafe extern "C" fn value_from_i64(value: i64) -> ValuePointer {
    value_to_value_pointer(Value::from_i64(value))
}

#[no_mangle]
pub unsafe extern "C" fn value_from_f64(value: f64) -> ValuePointer {
    value_to_value_pointer(Value::from_f64(value))
}

#[repr(C)]
pub struct InterpreterPointer {
    value: *mut c_void,
}

#[no_mangle]
pub unsafe extern "C" fn interpreter_load_file(name: *const c_char) -> InterpreterPointer {
    let file = c_str_to_string(name);
    let mut interp = gray::load_file(&file).expect("Failed to load file");
    built_in_functions::declare_functions(&mut interp);

    let boxed = Box::from(interp);
    InterpreterPointer { value: Box::into_raw(boxed) as *mut c_void }
}

#[no_mangle]
pub unsafe extern "C" fn interpreter_run(pointer: InterpreterPointer)  {
    let mut interp = Box::from_raw(pointer.value as *mut Interpreter);
    interp.run(None);
}

