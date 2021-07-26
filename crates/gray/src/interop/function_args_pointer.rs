use crate::interpreter::function_pointer::FunctionArgs;
use std::ffi::c_void;

#[repr(C)]
pub struct FunctionArgsPointer {
    value: *mut c_void,
}

impl From<FunctionArgs> for FunctionArgsPointer {
    fn from(value: FunctionArgs) -> Self {
        let boxed = Box::from(value);

        FunctionArgsPointer {
            value: Box::into_raw(boxed) as *mut c_void
        }
    }
}

impl From<FunctionArgsPointer> for FunctionArgs {
    fn from(pointer: FunctionArgsPointer) -> Self {
        unsafe {
            let boxed = Box::from_raw(pointer.value as *mut FunctionArgs);
            return *boxed;
        }
    }
}