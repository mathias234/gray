use crate::parser::parser::{ASTNode, ASTType, MathOp, ComparisonOp};
use crate::bytecode::code_block::CodeBlock;
use crate::bytecode::generator::Generator;
use std::collections::HashMap;
use crate::bytecode::register::Register;
use crate::interpreter::value::Value;
use crate::bytecode::instructions::other::{Return, Call, SetVariable, CompareEq, CompareNotEq, CompareLessThan, CompareGreaterThan, Store, LoadImmediate, GetVariable, PushScope, PopScope};
use crate::bytecode::instructions::jump::{JumpZero, Jump};
use crate::bytecode::instructions::math::{Add, Subtract, Multiply, Divide};

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

        compiler.compile_program_root(&root_node)?;


        Ok(compiler.blocks)
    }

    fn compile_program_root(&mut self, root: &ASTNode) -> Result<(), CompilerError> {
        for child in &root.children {
            match &child.ast_type {
                ASTType::Structure(name) => self.compile_structure(name, child),
                ASTType::Function(name) => self.compile_function(name, child),
                _ => Err(CompilerError::UnexpectedASTNode(child.clone())),
            }?;
        }

        Ok({})
    }

    fn compile_structure(&mut self, name: &str, node: &ASTNode) -> Result<(), CompilerError> {
        for child in &node.children {
            match &child.ast_type {
                ASTType::Function(function_name) => self.compile_function(&format!("{}::{}", name, function_name), child),
                _ => Err(CompilerError::UnexpectedASTNode(child.clone())),
            }?;
        }

        Ok({})
    }

    fn compile_function(&mut self, name: &str, node: &ASTNode) -> Result<(), CompilerError> {
        let mut generator = Generator::new();

        if node.children.len() > 0 {
            self.compile_scope(&mut generator, &node.children[0])?;
        }

        generator.emit(Return::new_boxed());

        self.blocks.insert(String::from(name), generator.block);
        Ok({})
    }

    fn compile_scope(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        generator.emit(PushScope::new_boxed());
        for child in &node.children {
            match &child.ast_type {
                ASTType::FunctionCall(call) => {
                    generator.emit(Call::new_boxed(&*call, None));
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
                _ => return Err(CompilerError::UnexpectedASTNode(child.clone())),
            }
        }

        generator.emit(PopScope::new_boxed());
        Ok({})
    }

    fn compile_variable_declaration(&mut self, variable: &String, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        self.compile_expression(generator, &node.children[0])?;
        generator.emit(SetVariable::new_boxed(variable.clone()));

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

    fn compile_expression(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let child = &node.children[0];
        match &child.ast_type {
            ASTType::MathExpression => self.compile_math_expression(generator, child),
            ASTType::ComparisonExpression => self.compile_comparison_expression(generator, child),
            ASTType::IntegerValue(_) => self.compile_value_to_accumulator(generator, child),
            ASTType::FloatValue(_) => self.compile_value_to_accumulator(generator, child),
            _ => Err(CompilerError::UnexpectedASTNode(child.clone())),
        }
    }

    fn compile_math_expression(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        let rhs_register = self.compile_value(generator, &node.children[2])?;
        self.compile_value_to_accumulator(generator, &node.children[0])?;

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
        println!("Dumping comp expression");
        node.dump(0);
        let rhs_register = self.compile_value(generator, &node.children[2])?;
        self.compile_value_to_accumulator(generator, &node.children[0])?;

        match &node.children[1].ast_type {
            ASTType::ComparisonOp(comp_op) => match comp_op {
                ComparisonOp::Equal => generator.emit(CompareEq::new_boxed(rhs_register)),
                ComparisonOp::NotEqual => generator.emit(CompareNotEq::new_boxed(rhs_register)),
                ComparisonOp::LessThan => generator.emit(CompareLessThan::new_boxed(rhs_register)),
                ComparisonOp::GreaterThan => generator.emit(CompareGreaterThan::new_boxed(rhs_register)),
                _ => return Err(CompilerError::UnexpectedASTNode(node.children[1].clone()))
            }
            _ => return Err(CompilerError::UnexpectedASTNode(node.children[1].clone()))
        }

        Ok({})
    }

    fn compile_value(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<Register, CompilerError> {
        let value_register = generator.next_free_register();

        self.compile_value_to_accumulator(generator, node)?;

        generator.emit(Store::new_boxed(value_register));

        Ok(value_register)
    }

    fn compile_value_to_accumulator(&mut self, generator: &mut Generator, node: &ASTNode) -> Result<(), CompilerError> {
        match &node.ast_type {
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