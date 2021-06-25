use crate::interpreter::interpreter::ExecutionContext;
use crate::bytecode::instructions::other::Instruction;
use crate::interpreter::value::{Value, DataValue};
use crate::interpreter::object::Object;
use crate::interpreter::array::Array;
use std::rc::Rc;
use crate::bytecode::register::Register;

pub struct CreateEmptyObject {}

#[allow(dead_code)]
impl CreateEmptyObject {
    pub fn new_boxed() -> Box<CreateEmptyObject> {
        Box::new(CreateEmptyObject {})
    }
}

pub struct SetObjectMember {
    object_register: Register,
    member: Rc<String>,
}

#[allow(dead_code)]
impl SetObjectMember {
    pub fn new_boxed(object_register: Register, member: String) -> Box<SetObjectMember> {
        Box::new(SetObjectMember {
            object_register,
            member: Rc::from(member),
        })
    }
}

pub struct GetObjectMember {
    object_register: Register,
    member: Rc<String>,
}

#[allow(dead_code)]
impl GetObjectMember {
    pub fn new_boxed(object_register: Register, member: String) -> Box<GetObjectMember> {
        Box::new(GetObjectMember {
            object_register,
            member: Rc::from(member),
        })
    }
}

pub struct CreateEmptyArray {}

#[allow(dead_code)]
impl CreateEmptyArray {
    pub fn new_boxed() -> Box<CreateEmptyArray> {
        Box::new(CreateEmptyArray {})
    }
}


impl Instruction for CreateEmptyObject {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_accumulator(Value::from_object(Object::new()));
    }

    fn to_string(&self) -> String { format!("CreateEmptyObject") }
}

impl Instruction for SetObjectMember {
    fn execute(&self, context: &mut ExecutionContext) {
        let mut obj = match context.get_register(&self.object_register).get_data_value() {
            DataValue::Object(object) => object.clone(),
            _ => { panic!("Trying to set a member value of something that is not an object") }
        };

        obj.declare(self.member.clone(), &context.get_accumulator());

        context.set_register(&self.object_register, Value::from_object(obj));
    }

    fn to_string(&self) -> String { format!("SetObjectMember {} {}", self.object_register, self.member) }
}

impl Instruction for GetObjectMember {
    fn execute(&self, context: &mut ExecutionContext) {
        let obj = match context.get_register(&self.object_register).get_data_value() {
            DataValue::Object(object) => object.clone(),
            _ => { panic!("Trying to get a member value of something that is not an object") }
        };

        context.set_accumulator(obj.get(self.member.clone()).unwrap());
    }

    fn to_string(&self) -> String { format!("GetObjectMember {} {}", self.object_register, self.member) }
}

impl Instruction for CreateEmptyArray {
    fn execute(&self, context: &mut ExecutionContext) {
        context.set_accumulator(Value::from_array(Array::new()));
    }

    fn to_string(&self) -> String { format!("CreateEmptyArray") }
}
