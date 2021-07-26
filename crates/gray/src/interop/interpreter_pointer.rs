use crate::interpreter::interpreter::Interpreter;
use std::ffi::c_void;

#[repr(C)]
pub struct InterpreterPointer {
    value: *mut c_void,
}

impl From<Interpreter<'_>> for InterpreterPointer {
    fn from(value: Interpreter) -> Self {
        let boxed = Box::from(value);

        InterpreterPointer {
            value: Box::into_raw(boxed) as *mut c_void
        }
    }
}

impl From<InterpreterPointer> for Interpreter<'_> {
    fn from(pointer: InterpreterPointer) -> Self {
        unsafe {
            let boxed = Box::from_raw(pointer.value as *mut Interpreter);
            return *boxed;
        }
    }
}