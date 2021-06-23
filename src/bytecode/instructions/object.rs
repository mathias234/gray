use crate::interpreter::interpreter::ExecutionContext;
use crate::bytecode::instructions::other::Instruction;
use crate::interpreter::value::{Value, DataValue};
use crate::interpreter::object::Object;
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