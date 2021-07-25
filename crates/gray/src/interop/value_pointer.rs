use crate::interpreter::value::Value;
use std::ffi::c_void;

#[repr(C)]
pub struct ValuePointer {
    value: *mut c_void,
}

impl From<Value> for ValuePointer {
    fn from(value: Value) -> Self {
        let boxed = Box::from(value);

        ValuePointer {
            value: Box::into_raw(boxed) as *mut c_void
        }
    }
}

impl From<ValuePointer> for Value {
    fn from(pointer: ValuePointer) -> Self {
        unsafe {
            let boxed = Box::from_raw(pointer.value as *mut Value);
            return *boxed;
        }
    }
}