use crate::bytecode::label::Label;
use crate::{
    bytecode::register::Register,
    interpreter::{interpreter::VariableHandle, value::Value},
};

pub enum Instruction {
    NotAnInstruction,
    LoadImmediate(Value),
    LoadRegister(Register),
    LoadArgument(usize),
    ParamsList(usize),
    Store(Register),
    Call(CallArgs),
    Return,
    DeclareVariable(VariableHandle),
    SetVariable(VariableHandle),
    GetVariable(VariableHandle),
    PushScope,
    PopScope,
    PushBreakContinueScope(Label /* Break */, Label /* Continue */),
    PopBreakContinueScope,
    Break,
    Continue,
    Range(Register),
    CreateIterator,
    IteratorGetNext,
    IteratorEmpty,
    NegateValue,
    CompareEq(Register),
    CompareNotEq(Register),
    CompareGreaterThan(Register),
    CompareLessThan(Register),
    CompareLessThanOrEqual(Register),
    CompareGreaterThanOrEqual(Register),
    And(Register),
    Or(Register),
    Jump(Label),
    JumpZero(Label),
    JumpNotZero(Label),
    Add(Register),
    Subtract(Register),
    Multiply(Register),
    Divide(Register),
    CreateEmptyObject,
    SetObjectMember(Register /* Object */, Register /* Accessor */),
    GetObjectMember(Register /* Object */, Register /* Accessor */),
    CreateEmptyArray,
    PushArray(Register /* Array */),
    GetArrayLength,
    GetArray(Register /* Array */),
    SetArray(Register /* Array */, Register /* Value */),
}

pub struct CallArgs {
    pub block_id: VariableHandle,
    pub arguments: Vec<Register>,
    pub pass_through_args: bool,
}

impl CallArgs {
    pub fn new(block_id: VariableHandle, args: Vec<Register>, pass_through_args: bool) -> CallArgs {
        CallArgs {
            block_id,
            arguments: args,
            pass_through_args,
        }
    }
}
