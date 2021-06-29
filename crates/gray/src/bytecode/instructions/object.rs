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


pub struct PushArray {
    array: Register,
}

#[allow(dead_code)]
impl PushArray {
    pub fn new_boxed(array: Register) -> Box<PushArray> {
        Box::new(PushArray { array })
    }
}

pub struct GetArray {
    array: Register,
}

#[allow(dead_code)]
impl GetArray {
    pub fn new_boxed(array: Register) -> Box<GetArray> {
        Box::new(GetArray { array })
    }
}

pub struct ArraySet {
    array: Register,
    value: Register,
}

#[allow(dead_code)]
impl ArraySet {
    pub fn new_boxed(array: Register, value: Register) -> Box<ArraySet> {
        Box::new(ArraySet { array, value })
    }
}


pub struct NotAnInstruction {}

#[allow(dead_code)]
impl NotAnInstruction {
    pub fn new_boxed() -> Box<NotAnInstruction> {
        Box::new(NotAnInstruction {})
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

impl Instruction for PushArray {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();
        let mut array = context.get_register(&self.array);
        match array.get_data_value_mut() {
            DataValue::Array(a) => {
                a.push(value);
            }
            _ => panic!("Error pushing to value that is not an array"),
        }
    }

    fn to_string(&self) -> String { format!("PushArray {}", self.array) }
}

impl Instruction for GetArray {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();
        let mut array = context.get_register(&self.array);

        let index = match value.get_data_value() {
            DataValue::I64(v) => *v as usize,
            d => panic!("Array can only be indexed with an integer {:?}", d),
        };

        match array.get_data_value_mut() {
            DataValue::Array(a) => {
                context.set_accumulator(a.get(index));
            }
            _ => panic!("Error getting value from object that is not an array"),
        }
    }

    fn to_string(&self) -> String { format!("GetArray {}", self.array) }
}

impl Instruction for ArraySet {
    fn execute(&self, context: &mut ExecutionContext) {
        let value = context.get_accumulator();
        let mut array = context.get_register(&self.array);

        let index = match value.get_data_value() {
            DataValue::I64(v) => *v as usize,
            d => panic!("Array can only be indexed with an integer {:?}", d),
        };

        match array.get_data_value_mut() {
            DataValue::Array(a) => {
                a.set(index, context.get_register(&self.value));
            }
            d => panic!("Error getting value from object that is not an array {:?}", d),
        }
    }

    fn to_string(&self) -> String { format!("SetArray {}", self.array) }
}

impl Instruction for NotAnInstruction {
    fn execute(&self, _: &mut ExecutionContext) {

    }

    fn to_string(&self) -> String { format!("NotAnInstruction") }
}
