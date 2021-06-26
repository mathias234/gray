use crate::parser::lexer::{TokenStream, Token, Keyword, TokenType};
use crate::parser::lexer::Delimiter;

#[derive(PartialEq, Debug, Clone)]
pub enum ExpressionOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ASTType {
    Scope,
    Expression,
    ReturnExpression,
    IfStatement,
    WhileStatement,
    Namespace(String),
    ExpressionOp(ExpressionOp),
    Function(String),
    FunctionCall(String),
    VariableDeclaration(String),
    VariableAssignment,
    FloatValue(f64),
    IntegerValue(i64),
    StringValue(String),
    Identifier(String),
    CreateObject,
    CreateArray,
    ObjectMember(String),
    ObjectAccess(String),
    Trait(String),
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEndOfProgram,
    UnexpectedKeywordInStream(Keyword),
    UnexpectedDelimiterInStream(Delimiter, Delimiter),
    UnexpectedTokenInStream(Token),
    UnexpectedTokenInStreamWithExpected(TokenType, Token),
    UnexpectedTokenInStreamExpectedIdentifier(Token),
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

        let root = parser.parse_scope()?;

        return Ok(root);
    }

    fn parse_scope(&mut self) -> Result<ASTNode, ParserError> {
        let mut scope = ASTNode::new(ASTType::Scope);

        let mut token;

        loop {
            let token_result = self.peek_next_token(0);

            match token_result {
                Ok(t) => token = t,
                Err(e) => match e {
                    ParserError::UnexpectedEndOfProgram => return Ok(scope),
                    _ => return Err(e),
                }
            }

            let child = match &token.token_type {
                TokenType::Keyword(keyword) => {
                    match keyword {
                        Keyword::Function => self.parse_function(),
                        Keyword::VariableDeclaration => self.parse_variable_declaration(),
                        Keyword::IfStatement => self.parse_if_statement(),
                        Keyword::WhileStatement => self.parse_while_statement(),
                        Keyword::Return => {
                            self.get_next_token()?;
                            let mut return_node = ASTNode::new(ASTType::ReturnExpression);
                            return_node.children.push(self.parse_expression()?);
                            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Semicolon)?;
                            Ok(return_node)
                        }
                        Keyword::Namespace => {
                            self.get_next_token()?;
                            let name_token = self.get_next_token()?;
                            let name = match &name_token.token_type {
                                TokenType::Identifier(name) => name.clone(),
                                _ => return Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(name_token.clone())),
                            };

                            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::OpenCurlyBracket)?;

                            let mut namespace = ASTNode::new(ASTType::Namespace(name));
                            namespace.children.push(self.parse_scope()?);
                            Ok(namespace)
                        }
                        _ => Err(ParserError::UnexpectedKeywordInStream(keyword.clone())),
                    }
                }
                TokenType::Delimiter(d) => {
                    match d {
                        Delimiter::CloseCurlyBracket => {
                            self.get_next_token()?;
                            return Ok(scope);
                        }
                        _ => Err(ParserError::UnexpectedDelimiterInStream(Delimiter::CloseCurlyBracket, d.clone())),
                    }
                }
                _ => {
                    let delimiter = self.peek_next_token(1)?;
                    let result;

                    if Parser::token_is_delimiter(delimiter, Delimiter::Equal) ||
                        Parser::token_is_delimiter(delimiter, Delimiter::Dot) {
                        result = self.parse_variable_assignment()?;
                        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Semicolon)?;
                    } else {
                        result = self.parse_expression()?;
                        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Semicolon)?;
                    }

                    Ok(result)
                }
            }?;

            scope.children.push(child);
        }
    }

    fn parse_function(&mut self) -> Result<ASTNode, ParserError> {
        self.get_next_token()?;

        let func_name;
        let name_token = self.get_next_token()?;

        match &name_token.token_type {
            TokenType::Identifier(name) => func_name = String::from(name),
            _ => return Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(name_token.clone())),
        }

        let mut node = ASTNode::new(ASTType::Function(func_name));

        let token = self.get_next_token()?;
        Parser::validate_token_is_delimiter(token, Delimiter::OpenParen)?;

        loop {
            let token = self.get_next_token()?;
            match &token.token_type {
                TokenType::Identifier(identifier) => {
                    let identifier = identifier.clone();
                    node.children.push(ASTNode::new(ASTType::Identifier(identifier)));
                }
                TokenType::Delimiter(d) => match d {
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
        }

        return Err(ParserError::UnexpectedTokenInStream(token.clone()));
    }

    fn parse_variable_declaration(&mut self) -> Result<ASTNode, ParserError> {
        self.get_next_token()?;

        let variable_name;
        let name_token = self.get_next_token()?;

        match &name_token.token_type {
            TokenType::Identifier(name) => variable_name = String::from(name),
            _ => return Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(name_token.clone())),
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
        self.get_next_token()?;

        let mut node = ASTNode::new(ASTType::IfStatement);
        let condition = self.parse_expression()?;

        node.children.push(condition);

        let open_curly = self.get_next_token()?;
        Parser::validate_token_is_delimiter(open_curly, Delimiter::OpenCurlyBracket)?;

        node.children.push(self.parse_scope()?);

        Ok(node)
    }

    fn parse_while_statement(&mut self) -> Result<ASTNode, ParserError> {
        self.get_next_token()?;

        let mut node = ASTNode::new(ASTType::WhileStatement);
        let condition = self.parse_expression()?;

        node.children.push(condition);

        let open_curly = self.get_next_token()?;
        Parser::validate_token_is_delimiter(open_curly, Delimiter::OpenCurlyBracket)?;

        node.children.push(self.parse_scope()?);

        Ok(node)
    }

    fn parse_expression(&mut self) -> Result<ASTNode, ParserError> {
        let next = self.peek_next_token(0)?;

        let lhs;
        if Parser::token_is_delimiter(next, Delimiter::OpenParen) {
            self.get_next_token()?;
            lhs = self.parse_expression()?;
            self.get_next_token()?;
        } else {
            lhs = self.parse_sub_expression()?;
        }

        let mut node = ASTNode::new(ASTType::Expression);

        node.children.push(lhs);

        let operator;

        let next_expression_operator = self.get_next_expression_operator()?;
        if next_expression_operator.is_some() {
            operator = ASTNode::new(ASTType::ExpressionOp(next_expression_operator.unwrap()));
            node.children.push(operator.clone());
            let rhs = self.parse_expression()?;
            node.children.push(rhs);


            let my_precedence = Parser::operator_precedence(&operator);

            let child_expr = &node.children[2];

            if child_expr.children.len() == 3 {
                match &child_expr.ast_type {
                    ASTType::Expression => {
                        let child_precedence = Parser::operator_precedence(&child_expr.children[1]);
                        if child_precedence < my_precedence {
                            // Kinda messy because of all the .children[]
                            // but essentially we are just rotating the tree to the left
                            let old_operator = node.children[1].clone();
                            node.children[1] = node.children[2].children[1].clone();
                            node.children[2].children[1] = old_operator;

                            let old_lhs = node.children[0].clone();
                            let child_old_lhs = node.children[2].children[0].clone();
                            let child_old_rhs = node.children[2].children[2].clone();

                            node.children[0] = node.children[2].clone();
                            node.children[0].children[0] = old_lhs;
                            node.children[0].children[2] = child_old_lhs;
                            node.children[2] = child_old_rhs;
                        }
                    }
                    _ => {}
                }
            }
        }

        Ok(node)
    }

    fn parse_sub_expression(&mut self) -> Result<ASTNode, ParserError> {
        let first_delimiter = self.peek_next_token(0)?;
        let delimiter = self.peek_next_token(1)?;

        return if Parser::token_is_delimiter(&delimiter, Delimiter::OpenParen) || Parser::token_is_delimiter(&delimiter, Delimiter::Colon) {
            Ok(self.parse_function_call()?)
        } else if Parser::token_is_delimiter(&first_delimiter, Delimiter::OpenCurlyBracket) {
            Ok(self.parse_object_declaration()?)
        } else if Parser::token_is_delimiter(&first_delimiter, Delimiter::OpenBracket) {
            Ok(self.parse_array_declaration()?)
        } else if Parser::token_is_delimiter(delimiter, Delimiter::Dot) {
            Ok(self.parse_member_expression()?)
        } else {
            // Very simple single token expression
            let token = self.get_next_token()?;
            Ok(Parser::token_to_simple_ast_node(&token)?)
        };
    }

    fn parse_array_declaration(&mut self) -> Result<ASTNode, ParserError> {
        let mut array_node = ASTNode::new(ASTType::CreateArray);

        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::OpenBracket)?;

        while !Parser::token_is_delimiter(self.peek_next_token(0)?, Delimiter::CloseBracket) {
            let expression = self.parse_expression()?;

            // If we do not end with comma we can assume that the object is fully declared
            if Parser::token_is_delimiter(self.peek_next_token(0)?, Delimiter::Comma) {
                self.get_next_token()?;
            } else {
                array_node.children.push(expression);
                break;
            }

            array_node.children.push(expression);
        }


        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::CloseBracket)?;

        Ok(array_node)
    }

    fn parse_object_declaration(&mut self) -> Result<ASTNode, ParserError> {
        let mut object_node = ASTNode::new(ASTType::CreateObject);

        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::OpenCurlyBracket)?;
        while !Parser::token_is_delimiter(self.peek_next_token(0)?, Delimiter::CloseCurlyBracket) {
            let token = self.get_next_token()?;
            let identifier = match &token.token_type {
                TokenType::Identifier(identifier) => Ok(identifier.clone()),
                _ => Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(token.clone()))
            }?;


            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Colon)?;
            let expression = self.parse_expression()?;

            let mut object_member = ASTNode::new(ASTType::ObjectMember(identifier));

            object_member.children.push(expression);

            // If we do not end with comma we can assume that the object is fully declared
            if Parser::token_is_delimiter(self.peek_next_token(0)?, Delimiter::Comma) {
                self.get_next_token()?;
            } else {
                object_node.children.push(object_member);
                break;
            }

            object_node.children.push(object_member);
        }

        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::CloseCurlyBracket)?;

        Ok(object_node)
    }

    fn parse_member_expression(&mut self) -> Result<ASTNode, ParserError> {
        let name = self.get_next_token()?;
        let name = match &name.token_type {
            TokenType::Identifier(n) => Ok(n.clone()),
            _ => Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(name.clone()))
        }?;

        self.get_next_token()?;

        let mut node = ASTNode::new(ASTType::ObjectAccess(name));

        if Parser::token_is_delimiter(self.peek_next_token(1)?, Delimiter::Dot) {
            node.children.push(self.parse_member_expression()?);
        } else {
            node.children.push(Parser::token_to_simple_ast_node(self.get_next_token()?)?);
        }

        Ok(node)
    }

    fn parse_variable_assignment(&mut self) -> Result<ASTNode, ParserError> {
        let lhs;

        if Parser::token_is_delimiter(self.peek_next_token(1)?, Delimiter::Dot) {
            lhs = self.parse_member_expression()?;
        } else {
            lhs = Parser::token_to_simple_ast_node(self.get_next_token()?)?;
        }

        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Equal)?;

        let mut assigment_node = ASTNode::new(ASTType::VariableAssignment);
        assigment_node.children.push(lhs);

        assigment_node.children.push(self.parse_expression()?);

        Ok(assigment_node)
    }

    fn parse_function_call(&mut self) -> Result<ASTNode, ParserError> {
        let mut namespace = String::new();

        while Parser::token_is_delimiter(self.peek_next_token(1)?, Delimiter::Colon) {
            let namespace_part = self.get_next_token()?;
            let namespace_part = match &namespace_part.token_type {
                TokenType::Identifier(identifier) => identifier.clone(),
                _ => return Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(namespace_part.clone())),
            };

            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Colon)?;
            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Colon)?;

            namespace += &format!("{}::", namespace_part);
        }

        let identifier = self.get_next_token()?;
        let mut identifier = match &identifier.token_type {
            TokenType::Identifier(identifier) => identifier.clone(),
            _ => return Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(identifier.clone())),
        };

        identifier = namespace + &identifier;

        let open_paren = self.get_next_token()?;
        Parser::validate_token_is_delimiter(open_paren, Delimiter::OpenParen)?;

        // Function Call
        let mut node = ASTNode::new(ASTType::FunctionCall(identifier));

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

        return Ok(node);
    }

    fn operator_precedence(node: &ASTNode) -> i32 {
        match &node.ast_type {
            ASTType::ExpressionOp(op) => {
                match op {
                    ExpressionOp::Add => 0,
                    ExpressionOp::Subtract => 0,
                    ExpressionOp::Multiply => 1,
                    ExpressionOp::Divide => 1,
                    ExpressionOp::Equal => 2,
                    ExpressionOp::NotEqual => 2,
                    ExpressionOp::LessThan => 2,
                    ExpressionOp::GreaterThan => 2,
                    ExpressionOp::LessThanOrEqual => 2,
                    ExpressionOp::GreaterThanOrEqual => 2,
                }
            }
            _ => -1,
        }
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
        return match &token.token_type {
            TokenType::Delimiter(d) => {
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
        match &token.token_type {
            TokenType::Identifier(identifier) => {
                Ok(ASTNode::new(ASTType::Identifier(identifier.clone())))
            }
            TokenType::String(value) => {
                Ok(ASTNode::new(ASTType::StringValue(value.clone())))
            }
            TokenType::Integer(value) => {
                Ok(ASTNode::new(ASTType::IntegerValue(*value)))
            }
            TokenType::Float(value) => {
                Ok(ASTNode::new(ASTType::FloatValue(*value)))
            }
            _ => Err(ParserError::UnexpectedTokenInStream(token.clone()))
        }
    }

    fn get_next_expression_operator(&mut self) -> Result<Option<ExpressionOp>, ParserError> {
        let token = self.peek_next_token(0)?;
        let token2 = match self.peek_next_token(1) {
            Ok(t) => Some(t),
            Err(_) => None,
        };

        if token2.is_some() && Parser::token_is_delimiter(token2.unwrap(), Delimiter::Equal) {
            if Parser::token_is_delimiter(&token, Delimiter::Equal) {
                self.get_next_token()?;
                self.get_next_token()?;
                return Ok(Some(ExpressionOp::Equal));
            } else if Parser::token_is_delimiter(&token, Delimiter::Exclamation) {
                self.get_next_token()?;
                self.get_next_token()?;
                return Ok(Some(ExpressionOp::NotEqual));
            } else if Parser::token_is_delimiter(&token, Delimiter::LessThan) {
                self.get_next_token()?;
                self.get_next_token()?;
                return Ok(Some(ExpressionOp::LessThanOrEqual));
            } else if Parser::token_is_delimiter(&token, Delimiter::GreaterThan) {
                self.get_next_token()?;
                self.get_next_token()?;
                return Ok(Some(ExpressionOp::GreaterThanOrEqual));
            }
        } else if Parser::token_is_delimiter(&token, Delimiter::LessThan) {
            self.get_next_token()?;
            return Ok(Some(ExpressionOp::LessThan));
        } else if Parser::token_is_delimiter(&token, Delimiter::GreaterThan) {
            self.get_next_token()?;
            return Ok(Some(ExpressionOp::GreaterThan));
        } else if Parser::token_is_delimiter(&token, Delimiter::Plus) {
            self.get_next_token()?;
            return Ok(Some(ExpressionOp::Add));
        } else if Parser::token_is_delimiter(&token, Delimiter::Hyphen) {
            self.get_next_token()?;
            return Ok(Some(ExpressionOp::Subtract));
        } else if Parser::token_is_delimiter(&token, Delimiter::Star) {
            self.get_next_token()?;
            return Ok(Some(ExpressionOp::Multiply));
        } else if Parser::token_is_delimiter(&token, Delimiter::Slash) {
            self.get_next_token()?;
            return Ok(Some(ExpressionOp::Divide));
        }

        return Ok(None);
    }

    fn validate_token_is_delimiter(token: &Token, delimiter: Delimiter) -> Result<(), ParserError> {
        match Parser::token_is_delimiter(token, delimiter.clone()) {
            true => Ok({}),
            false => match &token.token_type {
                TokenType::Delimiter(unexpected_delimiter) => {
                    Err(ParserError::UnexpectedDelimiterInStream(delimiter, unexpected_delimiter.clone()))
                }
                _ => {
                    Err(ParserError::UnexpectedTokenInStreamWithExpected(TokenType::Delimiter(delimiter), token.clone()))
                }
            }
        }
    }
}