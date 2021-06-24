use crate::parser::parser::{ASTNode, ASTType, MathOp, ComparisonOp};
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::generator::Generator;
use std::collections::HashMap;
use crate::bytecode::register::Register;
use crate::interpreter::value::Value;
use crate::bytecode::instructions::other::{Return, Call, DeclareVariable, Store, LoadImmediate, GetVariable, PushScope, PopScope, SetVariable, LoadArgument, LoadRegister};
use crate::bytecode::instructions::jump::{JumpZero, Jump};
use crate::bytecode::instructions::math::{Add, Subtract, Multiply, Divide};
use crate::bytecode::instructions::object::{CreateEmptyObject, SetObjectMember, GetObjectMember};
use crate::bytecode::instructions::comparison::{CompareGreaterThan, CompareLessThan, CompareNotEq, CompareEq, CompareLessThanOrEqual, CompareGreaterThanOrEqual};

#[derive(Debug)]
pub enum CompilerError {
    UnexpectedASTNode(ASTNode)
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
        compiler.compile_scope(&mut generator, &root_node)?;
        compiler.blocks.insert(String::from("ProgramMain"), generator.block);

        Ok(compiler.blocks)
    }

    fn compile_function(&mut self, name: &str, node: &ASTNode) -> Result<(), CompilerError> {
        let mut generator = Generator::new();

        let mut argument_index = 0;
        for child in &node.children {
            match &child.ast_type {
                ASTType::Scope => self.compile_scope(&mut generator, child)?,
                ASTType::Identifier(parameter) => {
                    generator.emit(LoadArgument::new_boxed(argument_index));
                    generator.emit(DeclareVariable::new_boxed(parameter.clone()));
                    argument_index += 1;
                }
                _ => return Err(CompilerError::UnexpectedASTNode(child.clone()))
            }
        }

        generator.emit(Return::new_boxed());

        self.blocks.insert(String::from(name), generator.block);
        Ok({})
    }

    fn compile_scope(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        generator.emit(PushScope::new_boxed());
        for child in &node.children {
            match &child.ast_type {
                ASTType::Expression => {
                    self.compile_expression(generator, child)?;
                }
                ASTType::VariableDeclaration(variable) => {
                    self.compile_variable_declaration(variable, generator, child)?;
                }
                ASTType::IfStatement => {
                    self.compile_if_statement(generator, child)?;
                }
                ASTType::WhileStatement => {
                    self.compile_while_statement(generator, child)?;
                }
                ASTType::ReturnExpression => {
                    self.compile_return(generator, child)?;
                }
                ASTType::VariableAssignment => {
                    self.compile_variable_assignment(generator, child)?
                }
                ASTType::Function(name) => self.compile_function(name, child)?,
                _ => return Err(CompilerError::UnexpectedASTNode(child.clone())),
            }
        }

        generator.emit(PopScope::new_boxed());
        Ok({})
    }

    fn compile_function_call(&mut self, call: &String, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        if node.children.len() == 0 {
            generator.emit(Call::new_boxed(&*call, None));
            return Ok({});
        }

        let mut argument_registers = Vec::new();

        for child in &node.children {
            self.compile_expression(generator, child)?;
            let register = generator.next_free_register();
            generator.emit(Store::new_boxed(register));
            argument_registers.push(register);
        }

        generator.emit(Call::new_boxed(&*call, Some(argument_registers)));


        Ok({})
    }

    fn compile_variable_declaration(&mut self, variable: &String, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        self.compile_expression(generator, &node.children[0])?;
        generator.emit(DeclareVariable::new_boxed(variable.clone()));

        Ok({})
    }

    fn compile_variable_assignment(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        self.compile_expression(generator, &node.children[1])?;
        let expression_result = generator.next_free_register();
        generator.emit(Store::new_boxed(expression_result));

        match &node.children[0].ast_type {
            ASTType::Identifier(identifier) => {
                generator.emit(LoadRegister::new_boxed(expression_result));
                generator.emit(SetVariable::new_boxed(identifier.clone()));
            }
            ASTType::ObjectAccess(name) => {
                generator.emit(GetVariable::new_boxed(name.clone()));
                let object_register = generator.next_free_register();
                generator.emit(Store::new_boxed(object_register));

                self.compile_object_access(generator, &node.children[0], object_register, expression_result)?;
            }
            _ => {}
        }

        Ok({})
    }

    fn compile_object_access(&mut self, generator: &mut Generator, node: &ASTNode, previous: Register, expression_result: Register) -> Result<(), CompilerError> {
        match &node.children[0].ast_type {
            ASTType::ObjectAccess(name) => {
                generator.emit(GetObjectMember::new_boxed(previous, name.clone()));
                let object_register = generator.next_free_register();
                generator.emit(Store::new_boxed(object_register));
                self.compile_object_access(generator, &node.children[0], object_register, expression_result)?;
            }
            ASTType::Identifier(name) => {
                generator.emit(LoadRegister::new_boxed(expression_result));
                generator.emit(SetObjectMember::new_boxed(previous, name.clone()));
            }
            _ => {}
        }


        Ok({})
    }


    fn compile_if_statement(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        self.compile_expression(generator, &node.children[0])?;

        let scope_start = generator.make_label();

        self.compile_scope(generator, &node.children[1])?;

        let mut scope_end = generator.make_label();
        scope_end.position += 1;

        generator.emit_at(JumpZero::new_boxed(scope_end), &scope_start);

        Ok({})
    }

    fn compile_while_statement(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let comparison_start = generator.make_label();
        self.compile_expression(generator, &node.children[0])?;

        let scope_start = generator.make_label();

        self.compile_scope(generator, &node.children[1])?;
        generator.emit(Jump::new_boxed(comparison_start));

        let mut scope_end = generator.make_label();
        scope_end.position += 1;

        generator.emit_at(JumpZero::new_boxed(scope_end), &scope_start);

        Ok({})
    }

    fn compile_return(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        // The last value in accumulator will be returned
        // and placed in the accumulator of the caller function

        self.compile_expression(generator, &node.children[0])?;

        generator.emit(PopScope::new_boxed());
        generator.emit(Return::new_boxed());

        Ok({})
    }

    fn compile_expression(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let child = &node.children[0];
        match &child.ast_type {
            ASTType::MathExpression => self.compile_math_expression(generator, child),
            ASTType::ComparisonExpression => self.compile_comparison_expression(generator, child),
            ASTType::IntegerValue(_) => self.compile_value_to_accumulator(generator, child),
            ASTType::FunctionCall(call) => self.compile_function_call(call, generator, child),
            ASTType::FloatValue(_) => self.compile_value_to_accumulator(generator, child),
            ASTType::Identifier(_) => self.compile_value_to_accumulator(generator, child),
            ASTType::StringValue(_) => self.compile_value_to_accumulator(generator, child),
            ASTType::CreateObject => self.compile_create_object(generator, child),
            _ => Err(CompilerError::UnexpectedASTNode(child.clone())),
        }
    }

    fn compile_create_object(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        generator.emit(CreateEmptyObject::new_boxed());
        let object_register = generator.next_free_register();
        generator.emit(Store::new_boxed(object_register));

        for member in &node.children {
            match &member.ast_type {
                ASTType::ObjectMember(name) => {
                    self.compile_expression(generator, &member.children[0])?;
                    generator.emit(SetObjectMember::new_boxed(object_register.clone(), name.clone()))
                }
                _ => return Err(CompilerError::UnexpectedASTNode(member.clone()))
            }
        }

        // Set the object into the accumulator
        generator.emit(LoadRegister::new_boxed(object_register));

        Ok({})
    }

    fn compile_math_expression(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        self.compile_expression(generator, &node.children[2])?;
        let rhs_register = generator.next_free_register();
        generator.emit(Store::new_boxed(rhs_register));

        self.compile_expression(generator, &node.children[0])?;

        match &node.children[1].ast_type {
            ASTType::MathOp(math_op) => match math_op {
                MathOp::Add => generator.emit(Add::new_boxed(rhs_register)),
                MathOp::Subtract => generator.emit(Subtract::new_boxed(rhs_register)),
                MathOp::Multiply => generator.emit(Multiply::new_boxed(rhs_register)),
                MathOp::Divide => generator.emit(Divide::new_boxed(rhs_register))
            }
            _ => return Err(CompilerError::UnexpectedASTNode(node.children[1].clone()))
        }

        Ok({})
    }

    fn compile_comparison_expression(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        self.compile_expression(generator, &node.children[2])?;
        let rhs_register = generator.next_free_register();
        generator.emit(Store::new_boxed(rhs_register));

        self.compile_expression(generator, &node.children[0])?;

        match &node.children[1].ast_type {
            ASTType::ComparisonOp(comp_op) => match comp_op {
                ComparisonOp::Equal => generator.emit(CompareEq::new_boxed(rhs_register)),
                ComparisonOp::NotEqual => generator.emit(CompareNotEq::new_boxed(rhs_register)),
                ComparisonOp::LessThan => generator.emit(CompareLessThan::new_boxed(rhs_register)),
                ComparisonOp::GreaterThan => generator.emit(CompareGreaterThan::new_boxed(rhs_register)),
                ComparisonOp::LessThanOrEqual => generator.emit(CompareLessThanOrEqual::new_boxed(rhs_register)),
                ComparisonOp::GreaterThanOrEqual => generator.emit(CompareGreaterThanOrEqual::new_boxed(rhs_register)),
            }
            _ => return Err(CompilerError::UnexpectedASTNode(node.children[1].clone()))
        }

        Ok({})
    }

    fn compile_value_to_accumulator(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        match &node.ast_type {
            ASTType::StringValue(value) => {
                Ok(generator.emit(LoadImmediate::new_boxed(Value::from_string(value.clone()))))
            }
            ASTType::IntegerValue(value) => {
                Ok(generator.emit(LoadImmediate::new_boxed(Value::from_i64(*value))))
            }
            ASTType::FloatValue(value) => {
                Ok(generator.emit(LoadImmediate::new_boxed(Value::from_f64(*value))))
            }
            ASTType::Identifier(identifier) => {
                Ok(generator.emit(GetVariable::new_boxed(identifier.clone())))
            }
            _ => Err(CompilerError::UnexpectedASTNode(node.clone())),
        }?;

        Ok({})
    }
}