use crate::parser::lexer::{TokenStream, Token, Keyword};
use crate::parser::lexer::Delimiter;

#[derive(PartialEq, Debug, Clone)]
pub enum MathOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ComparisonOp {
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ASTType {
    ProgramRoot,
    Scope,
    Expression,
    MathExpression,
    ComparisonExpression,
    IfStatement,
    WhileStatement,
    ComparisonOp(ComparisonOp),
    MathOp(MathOp),
    Function(String),
    Structure(String),
    FunctionCall(String),
    VariableDeclaration(String),
    VariableAssignment(String),
    FloatValue(f64),
    IntegerValue(i64),
    Identifier(String),
    Trait(String),
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEndOfProgram,
    UnexpectedKeywordInStream(Keyword),
    UnexpectedDelimiterInStream(Delimiter, Delimiter),
    UnexpectedTokenInStream(Token),
    DelimiterIsNotMathOperation(Delimiter),
    DelimiterIsNotComparisonOperation(Delimiter),
    UnimplementedFeature(&'static str),
}

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub ast_type: ASTType,
    pub children: Vec<ASTNode>,
}

impl ASTNode {
    pub fn new(ast_type: ASTType) -> ASTNode {
        ASTNode {
            ast_type,
            children: Vec::new(),
        }
    }

    pub fn dump(&self, indent: usize) {
        let mut indent_str = String::new();
        for _ in 0..indent {
            indent_str += "\t";
        }

        println!("{}{:?}", indent_str, self.ast_type);
        for child in &self.children {
            child.dump(indent + 1);
        }
    }
}

pub struct Parser {
    token_stream: TokenStream,
}


impl Parser {
    pub fn parse(token_stream: TokenStream) -> Result<ASTNode, ParserError> {
        let mut parser = Parser { token_stream };

        let mut root = parser.parse_scope()?;
        // Change it from Scope to ProgramRoot to be more explicit
        root.ast_type = ASTType::ProgramRoot;

        return Ok(root);
    }

    fn parse_scope(&mut self) -> Result<ASTNode, ParserError> {
        let mut scope = ASTNode::new(ASTType::Scope);

        let mut token;

        loop {
            let token_result = self.get_next_token();
            match token_result {
                Ok(t) => token = t,
                Err(e) => match e {
                    ParserError::UnexpectedEndOfProgram => return Ok(scope),
                    _ => return Err(e),
                }
            }

            let child = match token {
                Token::Keyword(keyword) => {
                    match keyword {
                        Keyword::Trait => self.parse_trait(),
                        Keyword::Structure => self.parse_structure(),
                        Keyword::Function => self.parse_function(),
                        Keyword::VariableDeclaration => self.parse_variable_declaration(),
                        Keyword::IfStatement => self.parse_if_statement(),
                        Keyword::WhileStatement => self.parse_while_statement(),
                        _ => Err(ParserError::UnexpectedKeywordInStream(keyword.clone())),
                    }
                }
                Token::Identifier(identifier) => {
                    let identifier = identifier.clone();
                    self.parse_identifier(identifier)
                }
                Token::Delimiter(d) => {
                    match d {
                        Delimiter::CloseCurlyBracket => return Ok(scope),
                        _ => Err(ParserError::UnexpectedDelimiterInStream(Delimiter::CloseCurlyBracket, d.clone())),
                    }
                }
                _ => Err(ParserError::UnexpectedTokenInStream(token.clone())),
            };

            scope.children.push(child?);
        }
    }

    fn parse_identifier(&mut self, identifier: String) -> Result<ASTNode, ParserError> {
        let mut built_identifier = String::from(identifier);

        let mut delimiter;
        loop {
            delimiter = self.get_next_token()?;

            if Parser::token_is_delimiter(delimiter, Delimiter::Dot) {
                // Member get
                return Err(ParserError::UnimplementedFeature("Member Get"));
            }

            if !Parser::token_is_delimiter(delimiter, Delimiter::Colon) { break; }

            let delimiter2 = self.get_next_token()?;
            if !Parser::token_is_delimiter(delimiter2, Delimiter::Colon) {
                return Err(ParserError::UnexpectedTokenInStream(delimiter2.clone()));
            }

            let identifier2 = self.get_next_token()?;
            let identifier2 = match identifier2 {
                Token::Identifier(identifier) => identifier,
                _ => return Err(ParserError::UnexpectedTokenInStream(identifier2.clone())),
            };


            built_identifier += &*format!("::{}", identifier2);
        }

        if Parser::token_is_delimiter(delimiter, Delimiter::OpenParen) {
            // Function Call
            let mut node = ASTNode::new(ASTType::FunctionCall(built_identifier));

            loop {
                let token = self.peek_next_token(0)?;
                if Parser::token_is_delimiter(token, Delimiter::CloseParen) {
                    self.get_next_token()?;
                    break;
                }

                node.children.push(self.parse_expression()?);

                let token = self.peek_next_token(0)?;
                if Parser::token_is_delimiter(token, Delimiter::Comma) {
                    self.get_next_token()?;
                }
            }


            let delimiter = self.get_next_token()?;
            Parser::validate_token_is_delimiter(delimiter, Delimiter::Semicolon)?;

            return Ok(node);
        }

        if Parser::token_is_delimiter(delimiter, Delimiter::Equal) {
            let mut node = ASTNode::new(ASTType::VariableAssignment(built_identifier));

            node.children.push(self.parse_expression()?);

            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Semicolon)?;

            return Ok(node);
        }

        Err(ParserError::UnimplementedFeature("parse_identifier Unknown"))
    }

    fn parse_trait(&mut self) -> Result<ASTNode, ParserError> {
        let trait_name;
        let name_token = self.get_next_token()?;

        match name_token {
            Token::Identifier(name) => trait_name = String::from(name),
            _ => return Err(ParserError::UnexpectedTokenInStream(name_token.clone())),
        }

        let mut node = ASTNode::new(ASTType::Trait(trait_name));

        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::OpenCurlyBracket)?;

        // Parse body
        loop {
            let token = self.get_next_token()?;
            if Parser::token_is_delimiter(token, Delimiter::CloseCurlyBracket) {
                break;
            }

            node.children.push(match token {
                Token::Keyword(keyword) => match keyword {
                    Keyword::Function => self.parse_function(),
                    _ => Err(ParserError::UnexpectedKeywordInStream(keyword.clone()))
                },
                _ => Err(ParserError::UnexpectedTokenInStream(token.clone())),
            }?);
        }


        Ok(node)
    }

    fn parse_structure(&mut self) -> Result<ASTNode, ParserError> {
        let struct_name;
        let name_token = self.get_next_token()?;

        match name_token {
            Token::Identifier(name) => struct_name = String::from(name),
            _ => return Err(ParserError::UnexpectedTokenInStream(name_token.clone())),
        }

        let mut node = ASTNode::new(ASTType::Structure(struct_name));

        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::OpenCurlyBracket)?;

        // Parse body
        loop {
            let token = self.get_next_token()?;
            if Parser::token_is_delimiter(token, Delimiter::CloseCurlyBracket) {
                break;
            }

            node.children.push(match token {
                Token::Keyword(keyword) => match keyword {
                    Keyword::Function => self.parse_function(),
                    _ => Err(ParserError::UnexpectedKeywordInStream(keyword.clone()))
                },
                _ => Err(ParserError::UnexpectedTokenInStream(token.clone())),
            }?);
        }


        Ok(node)
    }

    fn parse_function(&mut self) -> Result<ASTNode, ParserError> {
        let func_name;
        let name_token = self.get_next_token()?;

        match name_token {
            Token::Identifier(name) => func_name = String::from(name),
            _ => return Err(ParserError::UnexpectedTokenInStream(name_token.clone())),
        }

        let mut node = ASTNode::new(ASTType::Function(func_name));

        let token = self.get_next_token()?;
        Parser::validate_token_is_delimiter(token, Delimiter::OpenParen)?;

        loop {
            let token = self.get_next_token()?;
            match token {
                Token::Identifier(identifier) => {
                    let identifier = identifier.clone();
                    node.children.push(ASTNode::new(ASTType::Identifier(identifier)));
                }
                Token::Delimiter(d) => match d {
                    Delimiter::CloseParen => break,
                    _ => {}
                },
                _ => {}
            };

            let token = self.peek_next_token(0)?;
            if Parser::token_is_delimiter(token, Delimiter::Comma) {
                self.get_next_token()?;
            }
        }

        let token = self.get_next_token()?;
        if Parser::token_is_delimiter(token, Delimiter::Semicolon) {
            return Ok(node);
        } else if Parser::token_is_delimiter(token, Delimiter::OpenCurlyBracket) {
            node.children.push(self.parse_scope()?);
            return Ok(node);
        } else {
            return Err(ParserError::UnexpectedTokenInStream(token.clone()));
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<ASTNode, ParserError> {
        let variable_name;
        let name_token = self.get_next_token()?;

        match name_token {
            Token::Identifier(name) => variable_name = String::from(name),
            _ => return Err(ParserError::UnexpectedTokenInStream(name_token.clone())),
        }

        let mut node = ASTNode::new(ASTType::VariableDeclaration(variable_name));


        let delimiter = self.get_next_token()?;
        Parser::validate_token_is_delimiter(delimiter, Delimiter::Equal)?;

        let assignment_expression = self.parse_expression()?;
        node.children.push(assignment_expression);

        let semicolon = self.get_next_token()?;
        Parser::validate_token_is_delimiter(semicolon, Delimiter::Semicolon)?;


        Ok(node)
    }

    fn parse_if_statement(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = ASTNode::new(ASTType::IfStatement);
        let condition = self.parse_expression()?;

        node.children.push(condition);

        let open_curly = self.get_next_token()?;
        Parser::validate_token_is_delimiter(open_curly, Delimiter::OpenCurlyBracket)?;

        node.children.push(self.parse_scope()?);

        Ok(node)
    }

    fn parse_while_statement(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = ASTNode::new(ASTType::WhileStatement);
        let condition = self.parse_expression()?;

        node.children.push(condition);

        let open_curly = self.get_next_token()?;
        Parser::validate_token_is_delimiter(open_curly, Delimiter::OpenCurlyBracket)?;

        node.children.push(self.parse_scope()?);

        Ok(node)
    }

    fn parse_expression(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = ASTNode::new(ASTType::Expression);

        self.peek_next_token(0)?;
        let delimiter = self.peek_next_token(1)?;

        if !Parser::token_is_math_delimiter(&delimiter) && !Parser::token_is_boolean_delimiter(&delimiter) {
            // Very simple single token expression
            let token = self.get_next_token()?;
            node.children.push(Parser::token_to_simple_ast_node(&token)?);
        } else if Parser::token_is_math_delimiter(&delimiter) {
            node.children.push(self.parse_math_expression()?);
        } else if Parser::token_is_boolean_delimiter(&delimiter) {
            node.children.push(self.parse_comparison_expression()?);
        }

        Ok(node)
    }

    fn parse_math_expression(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = ASTNode::new(ASTType::MathExpression);
        let lhs_token = self.get_next_token()?;

        node.children.push(Parser::token_to_simple_ast_node(lhs_token)?);

        let math_op_token = self.get_next_token()?;
        node.children.push(Parser::token_to_math_op_ast_node(math_op_token)?);


        let rhs_token = self.peek_next_token(0)?;

        if Parser::token_is_delimiter(rhs_token, Delimiter::OpenParen) {
            node.children.push(self.parse_math_expression()?);
        } else {
            let rhs_token = self.get_next_token()?;
            node.children.push(Parser::token_to_simple_ast_node(rhs_token)?);
        }

        Ok(node)
    }

    fn parse_comparison_expression(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = ASTNode::new(ASTType::ComparisonExpression);

        let lhs_token = self.get_next_token()?;
        node.children.push(Parser::token_to_simple_ast_node(lhs_token)?);

        let comparison_op = self.get_next_token()?.clone();
        let token = self.get_next_token()?;

        let mut operator = ComparisonOp::Equal;

        let mut rhs = None;

        if Parser::token_is_delimiter(token, Delimiter::Equal) {
            if Parser::token_is_delimiter(&comparison_op, Delimiter::Equal) {
                operator = ComparisonOp::Equal;
            } else if Parser::token_is_delimiter(&comparison_op, Delimiter::Exclamation) {
                operator = ComparisonOp::NotEqual;
            } else if Parser::token_is_delimiter(&comparison_op, Delimiter::LessThan) {
                operator = ComparisonOp::LessThanOrEqual;
            } else if Parser::token_is_delimiter(&comparison_op, Delimiter::GreaterThan) {
                operator = ComparisonOp::GreaterThanOrEqual;
            } else {
                return match &comparison_op {
                    Token::Delimiter(delim) => Err(ParserError::DelimiterIsNotComparisonOperation(delim.clone())),
                    _ => Err(ParserError::UnexpectedTokenInStream(comparison_op))
                };
            }

            rhs = Some(self.get_next_token()?);
        } else if Parser::token_is_delimiter(&comparison_op, Delimiter::LessThan) {
            operator = ComparisonOp::LessThan;
            rhs = Some(token);
        } else if Parser::token_is_delimiter(&comparison_op, Delimiter::GreaterThan) {
            operator = ComparisonOp::GreaterThan;
            rhs = Some(token);
        }

        node.children.push(ASTNode::new(ASTType::ComparisonOp(operator)));

        match rhs {
            Some(rhs) => node.children.push(Parser::token_to_simple_ast_node(rhs)?),
            None => return Err(ParserError::UnimplementedFeature("parse_comparison_expression: This should not happen?")),
        }

        Ok(node)
    }


    fn get_next_token(&mut self) -> Result<&Token, ParserError> {
        let token = self.token_stream.next();
        match token {
            Some(token) => Ok(token),
            None => Err(ParserError::UnexpectedEndOfProgram)
        }
    }

    fn peek_next_token(&self, offset: usize) -> Result<&Token, ParserError> {
        let token = self.token_stream.peek_next(offset);
        match token {
            Some(token) => Ok(token),
            None => Err(ParserError::UnexpectedEndOfProgram)
        }
    }

    fn token_is_delimiter(token: &Token, delimiter: Delimiter) -> bool {
        return match token {
            Token::Delimiter(d) => {
                return if *d == delimiter {
                    true
                } else {
                    false
                };
            }
            _ => false
        };
    }

    fn token_to_simple_ast_node(token: &Token) -> Result<ASTNode, ParserError> {
        match token {
            Token::Identifier(identifier) => {
                Ok(ASTNode::new(ASTType::Identifier(identifier.clone())))
            }
            Token::Integer(value) => {
                Ok(ASTNode::new(ASTType::IntegerValue(*value)))
            }
            Token::Float(value) => {
                Ok(ASTNode::new(ASTType::FloatValue(*value)))
            }
            _ => Err(ParserError::UnexpectedTokenInStream(token.clone()))
        }
    }

    fn token_to_math_op_ast_node(token: &Token) -> Result<ASTNode, ParserError> {
        match token {
            Token::Delimiter(delimiter) => match delimiter {
                Delimiter::Plus => Ok(ASTNode::new(ASTType::MathOp(MathOp::Add))),
                Delimiter::Hyphen => Ok(ASTNode::new(ASTType::MathOp(MathOp::Subtract))),
                Delimiter::Star => Ok(ASTNode::new(ASTType::MathOp(MathOp::Multiply))),
                Delimiter::Slash => Ok(ASTNode::new(ASTType::MathOp(MathOp::Divide))),
                _ => Err(ParserError::DelimiterIsNotMathOperation(delimiter.clone()))
            }
            _ => Err(ParserError::UnexpectedTokenInStream(token.clone()))
        }
    }

    fn token_is_math_delimiter(token: &Token) -> bool {
        return match token {
            Token::Delimiter(d) => {
                match d {
                    Delimiter::Star => true,
                    Delimiter::Plus => true,
                    Delimiter::Hyphen => true,
                    Delimiter::Slash => true,
                    _ => false,
                }
            }
            _ => false
        };
    }

    fn token_is_boolean_delimiter(token: &Token) -> bool {
        return match token {
            Token::Delimiter(d) => {
                match d {
                    Delimiter::Equal => true,
                    Delimiter::GreaterThan => true,
                    Delimiter::LessThan => true,
                    Delimiter::Exclamation => true,
                    _ => false,
                }
            }
            _ => false
        };
    }

    fn validate_token_is_delimiter(token: &Token, delimiter: Delimiter) -> Result<(), ParserError> {
        match Parser::token_is_delimiter(token, delimiter.clone()) {
            true => Ok({}),
            false => match token {
                Token::Delimiter(unexpected_delimiter) => {
                    Err(ParserError::UnexpectedDelimiterInStream(delimiter, unexpected_delimiter.clone()))
                }
                _ => {
                    Err(ParserError::UnexpectedTokenInStream(token.clone()))
                }
            }
        }
    }
}