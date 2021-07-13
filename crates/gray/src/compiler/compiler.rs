use crate::parser::parser::{ASTNode, ASTType, ExpressionOp};
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::generator::Generator;
use std::collections::HashMap;
use crate::bytecode::register::Register;
use crate::interpreter::value::Value;
use crate::bytecode::instructions::other::{Return, Call, DeclareVariable, Store, LoadImmediate, GetVariable, PushScope, PopScope, SetVariable, LoadArgument, LoadRegister, Break, Continue, PopBreakContinueScope, PushBreakContinueScope};
use crate::bytecode::instructions::jump::{JumpZero, Jump};
use crate::bytecode::instructions::math::{Add, Subtract, Multiply, Divide};
use crate::bytecode::instructions::object::{CreateEmptyObject, SetObjectMember, GetObjectMember, CreateEmptyArray, PushArray, GetArray, ArraySet};
use crate::bytecode::instructions::comparison::{CompareGreaterThan, CompareLessThan, CompareNotEq, CompareEq, CompareLessThanOrEqual, CompareGreaterThanOrEqual, And, Or};
use std::rc::Rc;

#[derive(Debug)]
pub enum CompilerError {
    UnexpectedASTNode(ASTNode),
    ExpectedIdentifier(ASTNode),
}

pub struct Compiler {
    blocks: HashMap<String, CodeBlock>,
}

impl Compiler {
    pub fn compile(root_node: ASTNode) -> Result<HashMap<String, CodeBlock>, CompilerError> {
        let mut compiler = Compiler {
            blocks: HashMap::new(),
        };


        let mut generator = Generator::new();
        compiler.compile_scope("", &mut generator, &root_node, true)?;
        compiler.blocks.insert(String::from("ProgramMain"), generator.block);

        Ok(compiler.blocks)
    }

    fn compile_function(&mut self, namespace: &str, name: &str, node: &ASTNode) -> Result<(), CompilerError> {
        let mut generator = Generator::new();

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
                ASTType::Function(name) => self.compile_function(namespace, name, child)?,
                _ => return Err(CompilerError::UnexpectedASTNode(child.clone())),
            }
        }

        if push_scope {
            generator.emit(PopScope::new_boxed(), node.code_segment);
        }

        Ok({})
    }

    fn compile_function_call(&mut self, call: &String, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        if node.children.len() == 0 {
            generator.emit(Call::new_boxed(&*call, None), node.code_segment);
            return Ok({});
        }

        let mut argument_registers = Vec::new();

        for child in &node.children {
            self.compile_expression(generator, child)?;
            let register = generator.next_free_register();
            generator.emit(Store::new_boxed(register), child.code_segment);
            argument_registers.push(register);
        }

        generator.emit(Call::new_boxed(&call, Some(argument_registers.clone())), node.code_segment);

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

    fn compile_object_get(&mut self, generator: &mut Generator, node: &ASTNode, previous: Register) -> Result<(), CompilerError> {
        match &node.children[0].ast_type {
            ASTType::ObjectAccess(name) => {
                generator.emit(GetObjectMember::new_boxed(previous, name.clone()), node.children[0].code_segment);
                let object_register = generator.next_free_register();

                generator.emit(Store::new_boxed(object_register), node.children[0].code_segment);
                self.compile_object_get(generator, &node.children[0], object_register)?;
                generator.release_register(object_register);
            }
            ASTType::Identifier(name) => {
                generator.emit(GetObjectMember::new_boxed(previous, name.clone()), node.children[0].code_segment);
            }
            ASTType::Subscript(name) => {
                generator.emit(GetObjectMember::new_boxed(previous, name.clone()), node.children[0].code_segment);
                let array_register = generator.next_free_register();
                generator.emit(Store::new_boxed(array_register), node.children[0].code_segment);

                self.compile_expression(generator, &node.children[0])?;

                generator.emit(GetArray::new_boxed(array_register), node.children[0].code_segment);
            }
            _ => return Err(CompilerError::ExpectedIdentifier(node.children[0].clone()))
        }

        Ok({})
    }

    fn compile_object_access(&mut self, generator: &mut Generator, node: &ASTNode, previous: Register, expression_result: Register) -> Result<(), CompilerError> {
        match &node.children[0].ast_type {
            ASTType::ObjectAccess(name) => {
                generator.emit(GetObjectMember::new_boxed(previous, name.clone()), node.children[0].code_segment);
                let object_register = generator.next_free_register();

                generator.emit(Store::new_boxed(object_register), node.children[0].code_segment);
                self.compile_object_access(generator, &node.children[0], object_register, expression_result)?;

                generator.release_register(object_register);
            }
            ASTType::Identifier(name) => {
                generator.emit(LoadRegister::new_boxed(expression_result), node.children[0].code_segment);
                generator.emit(SetObjectMember::new_boxed(previous, name.clone()), node.children[0].code_segment);
            }
            ASTType::Subscript(name) => {
                generator.emit(GetObjectMember::new_boxed(previous, name.clone()), node.children[0].code_segment);
                let object_register = generator.next_free_register();
                generator.emit(Store::new_boxed(object_register), node.children[0].code_segment);

                self.compile_expression(generator, &node.children[0])?;

                generator.emit(ArraySet::new_boxed(object_register, expression_result), node.children[0].code_segment);
                generator.release_register(object_register);
            }
            _ => {}
        }


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

    fn compile_return(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        // The last value in accumulator will be returned
        // and placed in the accumulator of the caller function

        self.compile_expression(generator, &node.children[0])?;

        generator.emit(PopScope::new_boxed(), node.code_segment);
        generator.emit(Return::new_boxed(), node.code_segment);

        Ok({})
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
            ASTType::Subscript(name) => {
                let variable_handle = generator.next_variable_handle(name);
                generator.emit(GetVariable::new_boxed(variable_handle), node.code_segment);
                let array_register = generator.next_free_register();
                generator.emit(Store::new_boxed(array_register), node.code_segment);

                self.compile_expression(generator, &node.children[0])?;

                generator.emit(GetArray::new_boxed(array_register), node.code_segment);

                generator.release_register(array_register);

                Ok({})
            }
            ASTType::ObjectAccess2 => {
                self.compile_expression(generator, &node.children[1])?;
                let object_register = generator.next_free_register();
                generator.emit(Store::new_boxed(object_register), node.code_segment);

                self.compile_object_get(generator, node, object_register)?;


                Ok({})
            }
            ASTType::ObjectAccess(name) => {
                let variable_handle = generator.next_variable_handle(name);
                generator.emit(GetVariable::new_boxed(variable_handle), node.code_segment);
                let object_register = generator.next_free_register();
                generator.emit(Store::new_boxed(object_register), node.code_segment);
                self.compile_object_get(generator, node, object_register)?;
                generator.release_register(object_register);
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
            self.compile_sub_expression(generator, child)?;

            let operator = &node.children[1];

            match &operator.ast_type {
                ASTType::ExpressionOp(op) => match op {
                    ExpressionOp::Add => {
                        generator.emit(Add::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Subtract => {
                        generator.emit(Subtract::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Multiply => {
                        generator.emit(Multiply::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Divide => {
                        generator.emit(Divide::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Equal => {
                        generator.emit(CompareEq::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::NotEqual => {
                        generator.emit(CompareNotEq::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::LessThan => {
                        generator.emit(CompareLessThan::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::GreaterThan => {
                        generator.emit(CompareGreaterThan::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::LessThanOrEqual => {
                        generator.emit(CompareLessThanOrEqual::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::GreaterThanOrEqual => {
                        generator.emit(CompareGreaterThanOrEqual::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::And => {
                        generator.emit(And::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Or => {
                        generator.emit(Or::new_boxed(rhs_register), node.children[1].code_segment);
                    }
                    ExpressionOp::Assign => {
                        self.compile_assign(generator, op, &node.children[0], rhs_register)?;
                    }
                    ExpressionOp::AddAssign => {
                        self.compile_assign(generator, op, &node.children[0], rhs_register)?;
                    }
                    ExpressionOp::SubtractAssign => {
                        self.compile_assign(generator, op, &node.children[0], rhs_register)?;
                    }
                    ExpressionOp::MultiplyAssign => {
                        self.compile_assign(generator, op, &node.children[0], rhs_register)?;
                    }
                    ExpressionOp::DivideAssign => {
                        self.compile_assign(generator, op, &node.children[0], rhs_register)?;
                    }
                }
                _ => return Err(CompilerError::UnexpectedASTNode(operator.clone())),
            }

            generator.release_register(rhs_register);
        }

        Ok({})
    }

    fn compile_access(&mut self, generator: &mut Generator, node: &ASTNode, rhs_register: Register) -> Result<(), CompilerError> {
        match &node.ast_type {
            ASTType::Identifier(i) => {
                let variable_handle = generator.next_variable_handle(i);
                generator.emit(SetVariable::new_boxed(variable_handle), node.code_segment);
            }
            ASTType::ObjectAccess(i) => {
                let variable_handle = generator.next_variable_handle(i);
                generator.emit(GetVariable::new_boxed(variable_handle), node.code_segment);
                let object_register = generator.next_free_register();
                generator.emit(Store::new_boxed(object_register), node.code_segment);

                self.compile_object_access(generator, &node, object_register, rhs_register)?;
            }
            ASTType::Subscript(i) => {
                let variable_handle = generator.next_variable_handle(i);
                generator.emit(GetVariable::new_boxed(variable_handle), node.code_segment);
                let array_register = generator.next_free_register();
                generator.emit(Store::new_boxed(array_register), node.code_segment);

                self.compile_expression(generator, &node.children[0])?;

                generator.emit(ArraySet::new_boxed(array_register, rhs_register), node.code_segment);
            }
            _ => return Err(CompilerError::ExpectedIdentifier(node.clone())),
        };

        Ok({})
    }

    fn compile_assign(&mut self, generator: &mut Generator, op: &ExpressionOp, node: &ASTNode, rhs_register: Register) -> Result<(), CompilerError> {
        match op {
            ExpressionOp::Assign => {
                generator.emit(LoadRegister::new_boxed(rhs_register), node.code_segment);
                self.compile_access(generator, node, rhs_register)?;
            }
            ExpressionOp::AddAssign => {
                generator.emit(Add::new_boxed(rhs_register), node.code_segment);
                let rhs_register = generator.next_free_register();
                self.compile_access(generator, node, rhs_register)?;
                generator.release_register(rhs_register);
            }
            ExpressionOp::SubtractAssign => {
                generator.emit(Subtract::new_boxed(rhs_register), node.code_segment);
                let rhs_register = generator.next_free_register();
                self.compile_access(generator, node, rhs_register)?;
                generator.release_register(rhs_register);
            }
            ExpressionOp::MultiplyAssign => {
                generator.emit(Multiply::new_boxed(rhs_register), node.code_segment);
                let rhs_register = generator.next_free_register();
                self.compile_access(generator, node, rhs_register)?;
                generator.release_register(rhs_register);
            }
            ExpressionOp::DivideAssign => {
                generator.emit(Divide::new_boxed(rhs_register), node.code_segment);
                let rhs_register = generator.next_free_register();
                self.compile_access(generator, node, rhs_register)?;
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
                    self.compile_expression(generator, &member.children[0])?;
                    generator.emit(SetObjectMember::new_boxed(object_register.clone(), name.clone()), member.code_segment)
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