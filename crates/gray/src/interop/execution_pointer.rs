use crate::interpreter::interpreter::ExecutionContext;
use std::ffi::c_void;

#[repr(C)]
pub struct ExecutionContextPointer {
    value: *mut c_void,
}

impl From<ExecutionContext> for ExecutionContextPointer {
    fn from(value: ExecutionContext) -> Self {
        let boxed = Box::from(value);

        ExecutionContextPointer {
            value: Box::into_raw(boxed) as *mut c_void,
        }
    }
}

impl From<ExecutionContextPointer> for ExecutionContext {
    fn from(pointer: ExecutionContextPointer) -> Self {
        unsafe {
            let boxed = Box::from_raw(pointer.value as *mut ExecutionContext);
            return *boxed;
        }
    }
}
