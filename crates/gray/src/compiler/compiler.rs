use crate::parser::parser::{ASTNode, ASTType, ExpressionOp};
use crate::bytecode::code_block::{CodeBlock, CodeSegment};
use crate::bytecode::generator::Generator;
use std::collections::HashMap;
use crate::bytecode::register::Register;
use crate::interpreter::value::Value;
use crate::bytecode::instructions::other::{Return, Call, DeclareVariable, Store, LoadImmediate, GetVariable, PushScope, PopScope, SetVariable, LoadArgument, LoadRegister, Break, Continue, PopBreakContinueScope, PushBreakContinueScope, ParamsList, Range};
use crate::bytecode::instructions::jump::{JumpZero, Jump};
use crate::bytecode::instructions::math::{Add, Subtract, Multiply, Divide};
use crate::bytecode::instructions::object::{CreateEmptyObject, SetObjectMember, GetObjectMember, CreateEmptyArray, PushArray, GetArray, SetArray, GetArrayLength};
use crate::bytecode::instructions::comparison::{CompareGreaterThan, CompareLessThan, CompareNotEq, CompareEq, CompareLessThanOrEqual, CompareGreaterThanOrEqual, And, Or};
use std::rc::Rc;
use crate::compiler::compiler::CompilerError::UnexpectedASTNode;

use crate::interpreter::interpreter::ExecutionContext;
use crate::interpreter::function_pointer::FunctionArgs;

#[derive(Debug)]
pub enum CompilerError {
    UnexpectedASTNode(ASTNode),
    ExpectedIdentifier(ASTNode),
}

pub struct Compiler {
    blocks: HashMap<String, CodeBlock>,
    native_functions: Vec<NativeFunction>,
}


pub type FunctionPointer = fn(&ExecutionContext, FunctionArgs) -> Value;

#[derive(Clone)]
pub struct NativeFunction {
    pub namespace: Vec<String>,
    pub name: String,
    pub pointer: FunctionPointer,
}

impl NativeFunction {
    pub fn new(namespace: Vec<String>, name: String, pointer: FunctionPointer) -> NativeFunction {
        NativeFunction {
            namespace,
            name,
            pointer,
        }
    }

    pub fn full_name(&self) -> String {
        let mut full_name = String::new();
        for n in &self.namespace {
            full_name += &format!("{}::", n);
        }
        full_name += &self.name;

        full_name
    }
}

impl Compiler {
    pub fn compile(root_node: ASTNode, native_functions: Vec<NativeFunction>) -> Result<HashMap<String, CodeBlock>, CompilerError> {
        let mut compiler = Compiler {
            blocks: HashMap::new(),
            native_functions,
        };


        let mut generator = Generator::new(&compiler.native_functions);
        compiler.compile_scope("", &mut generator, &root_node, true)?;
        compiler.blocks.insert(String::from("ProgramMain"), generator.block);

        Ok(compiler.blocks)
    }

    fn compile_function(&mut self, namespace: &str, name: &str, node: &ASTNode) -> Result<(), CompilerError> {
        let mut generator = Generator::new(&self.native_functions);

        let mut argument_index = 0;
        for child in &node.children {
            match &child.ast_type {
                ASTType::Scope => self.compile_scope(namespace, &mut generator, child, true)?,
                ASTType::Identifier(parameter) => {
                    generator.emit(LoadArgument::new_boxed(argument_index), child.code_segment);
                    let parameter_handle = generator.next_variable_handle(parameter);
                    generator.emit(DeclareVariable::new_boxed(parameter_handle), child.code_segment);
                    argument_index += 1;
                }
                ASTType::ParamsList => {
                    generator.emit(ParamsList::new_boxed(argument_index), child.code_segment);
                    let parameter_handle = generator.next_variable_handle("params");
                    generator.emit(DeclareVariable::new_boxed(parameter_handle), child.code_segment);
                }
                _ => return Err(CompilerError::UnexpectedASTNode(child.clone()))
            }
        }

        generator.emit(Return::new_boxed(), node.code_segment);

        let full_name;
        if !namespace.is_empty() {
            full_name = format!("{}::{}", namespace, name);
        } else {
            full_name = String::from(name);
        }

        self.blocks.insert(full_name, generator.block);
        Ok({})
    }

    fn compile_scope(&mut self, namespace: &str, generator: &mut Generator, node: &ASTNode, push_scope: bool) -> Result<(), CompilerError> {
        if push_scope {
            generator.emit(PushScope::new_boxed(), node.code_segment);
        }

        // Hoist all the scope's function declarations to the top
        for child in &node.children {
            match &child.ast_type {
                ASTType::Function(name) => {
                    let full_name;
                    if !namespace.is_empty() {
                        full_name = format!("{}::{}", namespace, name);
                    } else {
                        full_name = String::from(name);
                    }


                    generator.emit(LoadImmediate::new_boxed(Value::from_function(Rc::from(full_name.clone()))), all_segments(child));

                    let variable = generator.next_variable_handle(&full_name);
                    generator.emit(DeclareVariable::new_boxed(variable), all_segments(child));
                }
                _ => {}
            }
        }

        for child in &node.children {
            match &child.ast_type {
                ASTType::Expression => {
                    self.compile_expression(generator, child)?;
                }
                ASTType::VariableDeclaration(variable) => {
                    self.compile_variable_declaration(variable, generator, child)?;
                }
                ASTType::IfStatement => {
                    self.compile_if_statement(namespace, generator, child)?;
                }
                ASTType::WhileStatement => {
                    self.compile_while_statement(namespace, generator, child)?;
                }
                ASTType::ForStatement => {
                    self.compile_for_statement(namespace, generator, child)?;
                }
                ASTType::BreakExpresssion => {
                    generator.emit(Break::new_boxed(), node.code_segment);
                }
                ASTType::ContinueExpresssion => {
                    generator.emit(Continue::new_boxed(), node.code_segment);
                }
                ASTType::ReturnExpression => {
                    self.compile_return(generator, child)?;
                }
                ASTType::Namespace(name) => {
                    let new_namespace;
                    if !namespace.is_empty() {
                        new_namespace = format!("{}::{}", namespace, name);
                    } else {
                        new_namespace = name.clone();
                    }

                    self.compile_scope(&new_namespace, generator, &child.children[0], false)?;
                }
                ASTType::Scope => {
                    self.compile_scope("", generator, child, true)?;
                }
                ASTType::Function(name) => {
                    self.compile_function(namespace, name, child)?
                }
                _ => return Err(CompilerError::UnexpectedASTNode(child.clone())),
            }
        }

        if push_scope {
            generator.emit(PopScope::new_boxed(), node.code_segment);
        }

        Ok({})
    }

    fn compile_function_call(&mut self, call: &String, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let handle = generator.next_variable_handle(call);

        if node.children.len() == 0 {
            generator.emit(Call::new_boxed(handle, None), node.code_segment);
            return Ok({});
        }

        let mut argument_registers = Vec::new();

        for child in &node.children {
            self.compile_expression(generator, child)?;
            let register = generator.next_free_register();
            generator.emit(Store::new_boxed(register), child.code_segment);
            argument_registers.push(register);
        }

        generator.emit(Call::new_boxed(handle, Some(argument_registers.clone())), node.code_segment);

        for register in argument_registers {
            generator.release_register(register);
        }


        Ok({})
    }

    fn compile_variable_declaration(&mut self, variable: &String, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        self.compile_expression(generator, &node.children[0])?;
        let variable_handle = generator.next_variable_handle(variable);
        generator.emit(DeclareVariable::new_boxed(variable_handle), node.code_segment);

        Ok({})
    }

    fn compile_if_statement(&mut self, namespace: &str, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        self.compile_expression(generator, &node.children[0])?;

        let scope_start = generator.make_instruction_holder();

        self.compile_scope(namespace, generator, &node.children[1], true)?;
        let else_block_start = generator.make_instruction_holder();

        let scope_end = generator.make_label();

        let mut else_block_end_instruction = None;
        if node.children.len() >= 3 {
            match node.children[2].ast_type {
                ASTType::IfStatement => {
                    self.compile_if_statement(namespace, generator, &node.children[2])?;
                    else_block_end_instruction = Some(generator.make_instruction_holder());
                }
                ASTType::Scope => {
                    self.compile_scope(namespace, generator, &node.children[2], true)?;
                    else_block_end_instruction = Some(generator.make_instruction_holder());
                }
                _ => return Err(CompilerError::UnexpectedASTNode(node.children[2].clone()))
            }
        }

        let else_block_end = generator.make_label();

        generator.emit_at(JumpZero::new_boxed(scope_end), &scope_start, node.code_segment);
        generator.emit_at(Jump::new_boxed(else_block_end), &else_block_start, node.code_segment);
        if else_block_end_instruction.is_some() {
            generator.emit_at(Jump::new_boxed(else_block_end), else_block_end_instruction.as_ref().unwrap(), node.code_segment);
        }

        Ok({})
    }

    fn compile_while_statement(&mut self, namespace: &str, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let push_break_continue_holder = generator.make_instruction_holder();
        let continue_label = generator.make_label();

        let comparison_start = generator.make_label();
        self.compile_expression(generator, &node.children[0])?;

        let scope_start = generator.make_instruction_holder();

        self.compile_scope(namespace, generator, &node.children[1], true)?;
        generator.emit(Jump::new_boxed(comparison_start), node.code_segment);

        let scope_end = generator.make_label();

        generator.emit_at(JumpZero::new_boxed(scope_end), &scope_start, node.code_segment);

        let break_label = generator.make_label();

        generator.emit(PopBreakContinueScope::new_boxed(), node.code_segment);

        generator.emit_at(PushBreakContinueScope::new_boxed(break_label, continue_label), &push_break_continue_holder, node.code_segment);

        Ok({})
    }

    fn compile_for_statement(&mut self, namespace: &str, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let push_break_continue_holder = generator.make_instruction_holder();

        generator.emit(PushScope::new_boxed(), all_segments(node));

        self.compile_expression(generator, &node.children[1])?;
        let array_register = generator.next_free_register();
        generator.emit(Store::new_boxed(array_register), all_segments(node));

        let iterator_register = generator.next_free_register();
        generator.emit(LoadImmediate::new_boxed(Value::from_i64(-1)), all_segments(node));
        generator.emit(Store::new_boxed(iterator_register), all_segments(node));


        let for_start = generator.make_label();

        generator.emit(LoadImmediate::new_boxed(Value::from_i64(1)), node.code_segment);
        generator.emit(Add::new_boxed(iterator_register), node.code_segment);
        generator.emit(Store::new_boxed(iterator_register), node.code_segment);

        generator.emit(LoadRegister::new_boxed(array_register), node.children[1].code_segment);
        generator.emit(GetArrayLength::new_boxed(), node.children[1].code_segment);
        generator.emit(CompareNotEq::new_boxed(iterator_register), node.children[1].code_segment);

        let scope_start = generator.make_instruction_holder();

        let identifier_handle = match &node.children[0].ast_type {
            ASTType::Identifier(ident) => generator.next_variable_handle(ident),
            _ => return Err(CompilerError::ExpectedIdentifier(node.children[0].clone()))
        };

        generator.emit(LoadRegister::new_boxed(iterator_register), all_segments(node));
        generator.emit(GetArray::new_boxed(array_register), all_segments(&node.children[1]));
        generator.emit(DeclareVariable::new_boxed(identifier_handle), all_segments(&node.children[0]));

        self.compile_scope(namespace, generator, &node.children[2], true)?;


        generator.emit(Jump::new_boxed(for_start), node.code_segment);

        let scope_end = generator.make_label();

        generator.emit_at(JumpZero::new_boxed(scope_end), &scope_start, node.code_segment);

        let break_label = generator.make_label();

        generator.emit(PopBreakContinueScope::new_boxed(), node.code_segment);
        generator.emit_at(PushBreakContinueScope::new_boxed(break_label, for_start), &push_break_continue_holder, node.code_segment);

        generator.release_register(array_register);
        generator.release_register(iterator_register);

        generator.emit(PopScope::new_boxed(), all_segments(node));

        Ok({})
    }

    fn compile_return(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        // The last value in accumulator will be returned
        // and placed in the accumulator of the caller function

        self.compile_expression(generator, &node.children[0])?;

        generator.emit(PopScope::new_boxed(), node.code_segment);
        generator.emit(Return::new_boxed(), node.code_segment);

        Ok({})
    }

    fn compile_subscript(&mut self, generator: &mut Generator, node: &ASTNode, mut parent: Option<Register>, parent_is_object: bool) -> Result<(Register, Register), CompilerError> {
        if parent.is_none() {
            self.compile_sub_expression(generator, node.children.last().unwrap())?;
            let array_register = generator.next_free_register();
            generator.emit(Store::new_boxed(array_register), node.code_segment);
            parent = Some(array_register);
        }

        self.compile_expression(generator, &node.children[0])?;
        let mut index = generator.next_free_register();
        generator.emit(Store::new_boxed(index), node.code_segment);
        let mut array = parent.unwrap();


        if node.children.len() > 1 {
            match &node.children[1].ast_type {
                ASTType::Subscript => {
                    if parent_is_object {
                        let parent_accessor = match &node.children[2].ast_type {
                            ASTType::Identifier(identifier) => {
                                generator.emit(LoadImmediate::new_boxed(Value::from_string(Rc::new(identifier.clone()))), node.children[2].code_segment);
                                let accessor_register = generator.next_free_register();
                                generator.emit(Store::new_boxed(accessor_register), node.children[2].code_segment);
                                accessor_register
                            }
                            _ => return Err(UnexpectedASTNode(node.children[2].clone())),
                        };


                        generator.emit(GetObjectMember::new_boxed(parent.unwrap(), parent_accessor), all_segments(node));
                        array = generator.next_free_register();
                        generator.emit(Store::new_boxed(array), all_segments(node));
                    }


                    let register = generator.next_free_register();

                    generator.emit(LoadRegister::new_boxed(index), node.code_segment);

                    generator.emit(GetArray::new_boxed(array), node.code_segment);
                    generator.emit(Store::new_boxed(register), node.code_segment);

                    let (idx, arr) = self.compile_subscript(generator, &node.children[1], Some(register), false)?;

                    generator.release_register(array);
                    generator.release_register(index);

                    index = idx;
                    array = arr;

                    generator.release_register(register);
                }
                ASTType::ObjectAccess => {
                    let (object, accessor, _) = self.compile_assign_object(generator, node, parent.unwrap())?;

                    generator.release_register(array);
                    generator.release_register(index);

                    index = accessor;
                    array = object;
                }
                ASTType::Identifier(identifier) => {
                    if parent_is_object {
                        let identifier_register = generator.next_free_register();
                        generator.emit(LoadImmediate::new_boxed(Value::from_string(Rc::new(identifier.clone()))), node.children[1].code_segment);
                        generator.emit(Store::new_boxed(identifier_register), node.code_segment);

                        generator.emit(GetObjectMember::new_boxed(parent.unwrap(), identifier_register), node.children[1].code_segment);

                        generator.release_register(array);

                        array = generator.next_free_register();
                        generator.emit(Store::new_boxed(array), node.children[1].code_segment);

                        generator.release_register(identifier_register);
                    }
                }
                _ => {}
            }
        }


        Ok((index, array))
    }

    fn compile_sub_expression(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        match &node.ast_type {
            ASTType::IntegerValue(_) => self.compile_value_to_accumulator(generator, node),
            ASTType::FunctionCall(call) => self.compile_function_call(call, generator, node),
            ASTType::FloatValue(_) => self.compile_value_to_accumulator(generator, node),
            ASTType::Identifier(_) => self.compile_value_to_accumulator(generator, node),
            ASTType::StringValue(_) => self.compile_value_to_accumulator(generator, node),
            ASTType::CreateObject => self.compile_create_object(generator, node),
            ASTType::CreateArray => self.compile_create_array(generator, node),
            ASTType::Expression => self.compile_expression(generator, node),
            ASTType::Subscript => {
                let (index, array) = self.compile_subscript(generator, node, None, false)?;
                generator.emit(LoadRegister::new_boxed(index), all_segments(node));
                generator.emit(GetArray::new_boxed(array), all_segments(node));

                generator.release_register(index);
                generator.release_register(array);
                Ok({})
            }
            ASTType::ObjectAccess => {
                let value_accessed = &node.children[1];

                self.compile_expression(generator, value_accessed)?;
                let mut object_register = generator.next_free_register();
                generator.emit(Store::new_boxed(object_register), node.code_segment);

                let accessor = &node.children[0];

                let accessor_register;
                let mut is_array = false;

                match &accessor.ast_type {
                    ASTType::Identifier(identifier) => {
                        generator.emit(LoadImmediate::new_boxed(Value::from_string(Rc::new(identifier.clone()))), accessor.code_segment);
                        accessor_register = generator.next_free_register();
                        generator.emit(Store::new_boxed(accessor_register), accessor.code_segment);
                    }
                    _ => {
                        let (obj, acc, arr) = self.compile_assign_object(generator, accessor, object_register)?;
                        object_register = obj;
                        accessor_register = acc;
                        is_array = arr;
                    }
                }

                if !is_array {
                    generator.emit(GetObjectMember::new_boxed(object_register, accessor_register), all_segments(node));
                } else {
                    generator.emit(LoadRegister::new_boxed(accessor_register), all_segments(node));
                    generator.emit(GetArray::new_boxed(object_register), all_segments(node));
                }

                generator.release_register(object_register);
                generator.release_register(accessor_register);

                Ok({})
            }
            _ => Err(CompilerError::UnexpectedASTNode(node.clone())),
        }
    }

    fn compile_expression(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        if node.children.len() == 1 {
            let child = &node.children[0];
            self.compile_sub_expression(generator, child)?;
        } else {
            let rhs = &node.children[2];
            self.compile_sub_expression(generator, rhs)?;
            let rhs_register = generator.next_free_register();
            generator.emit(Store::new_boxed(rhs_register), node.children[2].code_segment);

            let child = &node.children[0];

            let operator = &node.children[1];

            match &operator.ast_type {
                ASTType::ExpressionOp(op) => match op {
                    ExpressionOp::Add => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(Add::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Subtract => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(Subtract::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Multiply => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(Multiply::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Divide => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(Divide::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Equal => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(CompareEq::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::NotEqual => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(CompareNotEq::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::LessThan => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(CompareLessThan::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::GreaterThan => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(CompareGreaterThan::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::LessThanOrEqual => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(CompareLessThanOrEqual::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::GreaterThanOrEqual => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(CompareGreaterThanOrEqual::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::And => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(And::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Or => {
                        self.compile_sub_expression(generator, child)?;
                        generator.emit(Or::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::AddAssign => {
                        self.compile_sub_expression(generator, child)?;
                        self.compile_assign_expression(generator, op, &node.children[0], rhs_register)?;
                    }
                    ExpressionOp::SubtractAssign => {
                        self.compile_sub_expression(generator, child)?;
                        self.compile_assign_expression(generator, op, &node.children[0], rhs_register)?;
                    }
                    ExpressionOp::MultiplyAssign => {
                        self.compile_sub_expression(generator, child)?;
                        self.compile_assign_expression(generator, op, &node.children[0], rhs_register)?;
                    }
                    ExpressionOp::DivideAssign => {
                        self.compile_sub_expression(generator, child)?;
                        self.compile_assign_expression(generator, op, &node.children[0], rhs_register)?;
                    }
                    ExpressionOp::Assign => {
                        self.compile_assign_expression(generator, op, &node.children[0], rhs_register)?;
                    }
                    ExpressionOp::Range => {
                        self.compile_sub_expression(generator, child)?;
                        let lhs_register = generator.next_free_register();

                        generator.emit(Range::new_boxed(rhs_register), all_segments(node));

                        generator.release_register(lhs_register);
                    }
                }
                _ => return Err(CompilerError::UnexpectedASTNode(operator.clone())),
            }

            generator.release_register(rhs_register);
        }

        Ok({})
    }

    fn compile_assign_object(&mut self, generator: &mut Generator, node: &ASTNode, object: Register) -> Result<(Register, Register, bool), CompilerError> {
        match &node.ast_type {
            ASTType::ObjectAccess => {
                let value_accessed = &node.children[1].children[0];

                let obj = match &value_accessed.ast_type {
                    ASTType::Identifier(identifier) => {
                        let identifier_register = generator.next_free_register();
                        generator.emit(LoadImmediate::new_boxed(Value::from_string(Rc::new(identifier.clone()))), value_accessed.code_segment);
                        generator.emit(Store::new_boxed(identifier_register), node.code_segment);

                        generator.emit(GetObjectMember::new_boxed(object, identifier_register), value_accessed.code_segment);

                        generator.release_register(identifier_register);

                        let register = generator.next_free_register();
                        generator.emit(Store::new_boxed(register), value_accessed.code_segment);
                        Ok(register)
                    }
                    _ => { Err(CompilerError::UnexpectedASTNode(value_accessed.clone())) }
                }?;

                let accessor = &node.children[0];

                match &accessor.ast_type {
                    ASTType::Identifier(identifier) => {
                        let register = generator.next_free_register();
                        generator.emit(LoadImmediate::new_boxed(Value::from_string(Rc::new(identifier.clone()))), accessor.code_segment);
                        generator.emit(Store::new_boxed(register), node.code_segment);

                        generator.release_register(object);

                        Ok((obj, register, false))
                    }
                    _ => {
                        let result = self.compile_assign_object(generator, accessor, obj)?;

                        generator.release_register(obj);
                        generator.release_register(object);

                        Ok(result)
                    }
                }
            }
            ASTType::Subscript => {
                let (index, array) = self.compile_subscript(generator, node, Some(object), true)?;

                Ok((array, index, true))
            }
            _ => Err(CompilerError::UnexpectedASTNode(node.clone())),
        }
    }

    fn compile_assign(&mut self, generator: &mut Generator, node: &ASTNode, rhs_register: Register) -> Result<(), CompilerError> {
        match &node.ast_type {
            ASTType::Identifier(i) => {
                let variable_handle = generator.next_variable_handle(i);
                generator.emit(SetVariable::new_boxed(variable_handle), node.code_segment);
            }
            ASTType::ObjectAccess => {
                let value_accessed = &node.children[1];

                self.compile_expression(generator, value_accessed)?;
                let mut object_register = generator.next_free_register();
                generator.emit(Store::new_boxed(object_register), node.code_segment);

                let accessor = &node.children[0];

                let accessor_register;

                let mut is_array = false;

                match &accessor.ast_type {
                    ASTType::Identifier(identifier) => {
                        generator.emit(LoadImmediate::new_boxed(Value::from_string(Rc::new(identifier.clone()))), accessor.code_segment);
                        accessor_register = generator.next_free_register();
                        generator.emit(Store::new_boxed(accessor_register), accessor.code_segment);
                    }
                    _ => {
                        let (obj, acc, arr) = self.compile_assign_object(generator, accessor, object_register)?;
                        object_register = obj;
                        accessor_register = acc;
                        is_array = arr;
                    }
                }

                /* FIXME: SetObjectMember has it's value in the accumulator, while SetArray has the accessor in the accumulator.
                       Should make this more consistent */

                if !is_array {
                    generator.emit(LoadRegister::new_boxed(rhs_register), node.code_segment);
                    generator.emit(SetObjectMember::new_boxed(object_register, accessor_register), all_segments(node));
                } else {
                    generator.emit(LoadRegister::new_boxed(accessor_register), node.code_segment);
                    generator.emit(SetArray::new_boxed(object_register, rhs_register), node.code_segment);
                }

                generator.release_register(object_register);
                generator.release_register(accessor_register);
            }
            ASTType::Subscript => {
                let (index, array) = self.compile_subscript(generator, node, None, false)?;

                generator.emit(LoadRegister::new_boxed(index), all_segments(node));
                generator.emit(SetArray::new_boxed(array, rhs_register), all_segments(node));
            }
            _ => return Err(CompilerError::ExpectedIdentifier(node.clone())),
        };

        Ok({})
    }

    fn compile_assign_expression(&mut self, generator: &mut Generator, op: &ExpressionOp, node: &ASTNode, rhs_register: Register) -> Result<(), CompilerError> {
        match op {
            ExpressionOp::Assign => {
                generator.emit(LoadRegister::new_boxed(rhs_register), node.code_segment);
                self.compile_assign(generator, node, rhs_register)?;
            }
            ExpressionOp::AddAssign => {
                generator.emit(Add::new_boxed(rhs_register), node.code_segment);
                let rhs_register = generator.next_free_register();
                self.compile_assign(generator, node, rhs_register)?;
                generator.release_register(rhs_register);
            }
            ExpressionOp::SubtractAssign => {
                generator.emit(Subtract::new_boxed(rhs_register), node.code_segment);
                let rhs_register = generator.next_free_register();
                self.compile_assign(generator, node, rhs_register)?;
                generator.release_register(rhs_register);
            }
            ExpressionOp::MultiplyAssign => {
                generator.emit(Multiply::new_boxed(rhs_register), node.code_segment);
                let rhs_register = generator.next_free_register();
                self.compile_assign(generator, node, rhs_register)?;
                generator.release_register(rhs_register);
            }
            ExpressionOp::DivideAssign => {
                generator.emit(Divide::new_boxed(rhs_register), node.code_segment);
                let rhs_register = generator.next_free_register();
                self.compile_assign(generator, node, rhs_register)?;
                generator.release_register(rhs_register);
            }
            _ => {}
        }
        Ok({})
    }

    fn compile_create_array(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        generator.emit(CreateEmptyArray::new_boxed(), node.code_segment);
        let array_register = generator.next_free_register();
        generator.emit(Store::new_boxed(array_register), node.code_segment);

        for child in &node.children {
            self.compile_expression(generator, child)?;
            generator.emit(PushArray::new_boxed(array_register), node.code_segment);
        }


        generator.emit(LoadRegister::new_boxed(array_register), node.code_segment);

        generator.release_register(array_register);

        Ok({})
    }

    fn compile_create_object(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        generator.emit(CreateEmptyObject::new_boxed(), node.code_segment);
        let object_register = generator.next_free_register();
        generator.emit(Store::new_boxed(object_register), node.code_segment);

        for member in &node.children {
            match &member.ast_type {
                ASTType::ObjectMember(name) => {
                    let accessor = &node.children[0];

                    let accessor_register = generator.next_free_register();
                    generator.emit(LoadImmediate::new_boxed(Value::from_string(Rc::new(name.clone()))), accessor.code_segment);
                    generator.emit(Store::new_boxed(accessor_register), accessor.code_segment);

                    self.compile_expression(generator, &member.children[0])?;
                    generator.emit(SetObjectMember::new_boxed(object_register.clone(), accessor_register), member.code_segment);

                    generator.release_register(accessor_register);
                }
                _ => return Err(CompilerError::UnexpectedASTNode(member.clone()))
            }
        }

        // Set the object into the accumulator
        generator.emit(LoadRegister::new_boxed(object_register), node.code_segment);

        generator.release_register(object_register);

        Ok({})
    }

    fn compile_value_to_accumulator(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        match &node.ast_type {
            ASTType::StringValue(value) => {
                Ok(generator.emit(LoadImmediate::new_boxed(Value::from_string(Rc::new(value.clone()))), node.code_segment))
            }
            ASTType::IntegerValue(value) => {
                Ok(generator.emit(LoadImmediate::new_boxed(Value::from_i64(*value)), node.code_segment))
            }
            ASTType::FloatValue(value) => {
                Ok(generator.emit(LoadImmediate::new_boxed(Value::from_f64(*value)), node.code_segment))
            }
            ASTType::Identifier(identifier) => {
                let variable_handle = generator.next_variable_handle(identifier);
                Ok(generator.emit(GetVariable::new_boxed(variable_handle), node.code_segment))
            }
            _ => Err(CompilerError::UnexpectedASTNode(node.clone())),
        }?;

        Ok({})
    }
}

fn all_segments(node: &ASTNode) -> CodeSegment {
    let mut segment = node.code_segment;

    for child in &node.children {
        let child_segment = all_segments(child);

        if child_segment.start_x <= segment.start_x {
            segment.start_x = child_segment.start_x;
        }

        if child_segment.start_y <= segment.start_y {
            segment.start_y = child_segment.start_y;
        }

        if child_segment.end_x >= segment.end_x {
            segment.end_x = child_segment.end_x;
        }

        if child_segment.end_x >= segment.end_x {
            segment.end_x = child_segment.end_x;
        }
    }

    segment
}
