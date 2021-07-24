use crate::parser::lexer::{TokenStream, Token, Keyword, TokenType};
use crate::parser::lexer::Delimiter;
use crate::bytecode::code_block::CodeSegment;
use crate::error_printer;

#[derive(PartialEq, Debug, Clone)]
pub enum ExpressionOp {
    Add,
    Subtract,
    Multiply,
    Divide,

    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,

    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    And,
    Or,

    Range,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ASTType {
    Scope,
    Expression,
    ReturnExpression,
    ContinueExpresssion,
    BreakExpresssion,
    IfStatement,
    WhileStatement,
    ForStatement,
    Namespace(String),
    ExpressionOp(ExpressionOp),
    Function(String),
    FunctionCall(String),
    VariableDeclaration(String),
    FloatValue(f64),
    IntegerValue(i64),
    StringValue(String),
    Identifier(String),
    CreateObject,
    CreateArray,
    ObjectMember(String),
    ObjectAccess,
    Subscript,
    Trait(String),
    ParamsList,
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEndOfProgram,
    UnexpectedKeywordInStream(Keyword, Token),
    UnexpectedDelimiterInStream(Delimiter, Token),
    UnexpectedTokenInStream(Token),
    UnexpectedTokenInStreamWithExpected(TokenType, Token),
    UnexpectedTokenInStreamExpectedIdentifier(Token),
}

#[derive(Debug, Clone)]
pub struct ASTNode {
    pub ast_type: ASTType,
    pub children: Vec<ASTNode>,
    pub code_segment: CodeSegment,
}

impl ASTNode {
    pub fn new(ast_type: ASTType, code_segment: CodeSegment) -> ASTNode {
        ASTNode {
            ast_type,
            code_segment,
            children: Vec::new(),
        }
    }

    pub fn dump(&self, indent: usize) {
        let mut indent_str = String::new();
        for _ in 0..indent {
            indent_str += "\t";
        }

        println!("{}{:?}({:?})", indent_str, self.ast_type, self.code_segment);
        for child in &self.children {
            child.dump(indent + 1);
        }
    }
}

pub struct Parser {
    token_stream: TokenStream,
}

impl Parser {
    fn token_type_to_string(token_type: &TokenType) -> String {
        match token_type {
            TokenType::Identifier(i) => format!("identifier {}", i),
            TokenType::Integer(v) => format!("integer {}", v),
            TokenType::Float(v) => format!("float {}", v),
            TokenType::String(s) => format!("string {}", s),
            TokenType::Delimiter(d) => format!("delimiter {:?}", d),
            TokenType::Keyword(k) => format!("keyword {:?}", k),
            TokenType::EndOfFile => format!("End Of File"),
        }
    }

    fn pretty_print_error(&self, error: &ParserError, code_string: &str) {
        let lines: Vec<&str> = code_string.split('\n').collect();

        let mut code_segment = CodeSegment::new(1, 1, 1, 1);

        let error_message = match error {
            ParserError::UnexpectedTokenInStreamWithExpected(t, v) => {
                code_segment = v.position;
                format!("Unexpected {}, expected {}", Parser::token_type_to_string(&v.token_type), Parser::token_type_to_string(t))
            }
            ParserError::UnexpectedTokenInStream(v) => {
                code_segment = v.position;
                format!("Unexpected {}", Parser::token_type_to_string(&v.token_type))
            }
            ParserError::UnexpectedTokenInStreamExpectedIdentifier(v) => {
                code_segment = v.position;
                format!("Unexpected {}, expected an identifier", Parser::token_type_to_string(&v.token_type))
            }
            ParserError::UnexpectedDelimiterInStream(d, v) => {
                code_segment = v.position;
                format!("Unexpected {}, expected {:?}", Parser::token_type_to_string(&v.token_type), d)
            }
            ParserError::UnexpectedKeywordInStream(k, v) => {
                code_segment = v.position;
                format!("Unexpected {}, expected {:?}", Parser::token_type_to_string(&v.token_type), k)
            }
            ParserError::UnexpectedEndOfProgram => {
                format!("Unexpected end of program!")
            }
        };

        error_printer::print_error_line(
            lines[code_segment.start_y - 1],
            code_segment.start_y,
            code_segment.start_x,
            code_segment.end_x, );


        println!(" {}", error_message)
    }

    pub fn parse(token_stream: TokenStream, code_string: &str) -> Result<ASTNode, ParserError> {
        let mut parser = Parser { token_stream };

        let root = parser.parse_scope();

        if root.is_err() {
            parser.pretty_print_error(root.as_ref().unwrap_err(), code_string);
        }

        return root;
    }

    fn parse_scope(&mut self) -> Result<ASTNode, ParserError> {
        let mut scope = ASTNode::new(ASTType::Scope, CodeSegment::new(1, 1, 1, 1));

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
                        Keyword::ForStatement => self.parse_for_statement(),
                        Keyword::Return => {
                            let token = self.get_next_token()?;
                            let mut return_node = ASTNode::new(ASTType::ReturnExpression, token.position);
                            return_node.children.push(self.parse_expression()?);
                            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Semicolon)?;
                            Ok(return_node)
                        }
                        Keyword::Continue => {
                            let token = self.get_next_token()?;
                            let return_value = ASTNode::new(ASTType::ContinueExpresssion, token.position);
                            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Semicolon)?;
                            Ok(return_value)
                        }
                        Keyword::Break => {
                            let token = self.get_next_token()?;
                            let return_value = ASTNode::new(ASTType::BreakExpresssion, token.position);
                            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Semicolon)?;
                            Ok(return_value)
                        }
                        Keyword::Namespace => {
                            let token = self.get_next_token()?.clone();
                            let name_token = self.get_next_token()?;
                            let name = match &name_token.token_type {
                                TokenType::Identifier(name) => name.clone(),
                                _ => return Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(name_token.clone())),
                            };

                            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::OpenCurlyBracket)?;

                            let mut namespace = ASTNode::new(ASTType::Namespace(name), token.position);
                            namespace.children.push(self.parse_scope()?);
                            Ok(namespace)
                        }
                        _ => Err(ParserError::UnexpectedKeywordInStream(keyword.clone(), token.clone())),
                    }
                }
                TokenType::Delimiter(d) => {
                    match d {
                        Delimiter::OpenCurlyBracket => {
                            self.get_next_token()?;

                            self.parse_scope()
                        }
                        Delimiter::CloseCurlyBracket => {
                            self.get_next_token()?;
                            return Ok(scope);
                        }
                        _ => Err(ParserError::UnexpectedDelimiterInStream(Delimiter::CloseCurlyBracket, token.clone())),
                    }
                }
                _ => {
                    let result;

                    result = self.parse_expression()?;
                    Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Semicolon)?;

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

        let mut node = ASTNode::new(ASTType::Function(func_name), name_token.position);

        let token = self.get_next_token()?;
        Parser::validate_token_is_delimiter(token, Delimiter::OpenParen)?;

        loop {
            let token = self.get_next_token()?;
            match &token.token_type {
                TokenType::Identifier(identifier) => {
                    let identifier = identifier.clone();
                    if identifier == "params" {
                        node.children.push(ASTNode::new(ASTType::ParamsList, token.position));
                        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::CloseParen)?;
                        break;
                    }
                    node.children.push(ASTNode::new(ASTType::Identifier(identifier), token.position));
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

        let mut node = ASTNode::new(ASTType::VariableDeclaration(variable_name), name_token.position);


        let delimiter = self.get_next_token()?;
        Parser::validate_token_is_delimiter(delimiter, Delimiter::Equal)?;

        let assignment_expression = self.parse_expression()?;
        node.children.push(assignment_expression);

        let semicolon = self.get_next_token()?;
        Parser::validate_token_is_delimiter(semicolon, Delimiter::Semicolon)?;


        Ok(node)
    }

    fn parse_if_statement(&mut self) -> Result<ASTNode, ParserError> {
        let token = self.get_next_token()?;

        let mut node = ASTNode::new(ASTType::IfStatement, token.position);
        let condition = self.parse_expression()?;

        node.children.push(condition);

        let open_curly = self.get_next_token()?;
        Parser::validate_token_is_delimiter(open_curly, Delimiter::OpenCurlyBracket)?;

        node.children.push(self.parse_scope()?);

        if self.peek_next_token(0).is_err() {
            return Ok(node);
        }

        match &self.peek_next_token(0)?.token_type {
            TokenType::Keyword(keyword) => {
                match keyword {
                    Keyword::ElseStatement => {
                        self.get_next_token()?;

                        match &self.peek_next_token(0)?.token_type {
                            TokenType::Keyword(keyword) => {
                                match keyword {
                                    Keyword::IfStatement => {
                                        let else_if_statement = self.parse_if_statement()?;
                                        node.children.push(else_if_statement);
                                    }
                                    _ => {}
                                }
                            }
                            TokenType::Delimiter(delimiter) => {
                                match delimiter {
                                    Delimiter::OpenCurlyBracket => {
                                        self.get_next_token()?;
                                        let else_statement = self.parse_scope()?;
                                        node.children.push(else_statement);
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        Ok(node)
    }

    fn parse_while_statement(&mut self) -> Result<ASTNode, ParserError> {
        let token = self.get_next_token()?;

        let mut node = ASTNode::new(ASTType::WhileStatement, token.position);
        let condition = self.parse_expression()?;

        node.children.push(condition);

        let open_curly = self.get_next_token()?;
        Parser::validate_token_is_delimiter(open_curly, Delimiter::OpenCurlyBracket)?;

        node.children.push(self.parse_scope()?);

        Ok(node)
    }

    fn parse_for_statement(&mut self) -> Result<ASTNode, ParserError> {
        let token = self.get_next_token()?;

        let mut node = ASTNode::new(ASTType::ForStatement, token.position);

        let identifier = Parser::token_to_simple_ast_node(self.get_next_token()?)?;

        node.children.push(identifier);

        let in_keyword = self.get_next_token()?;
        match &in_keyword.token_type {
            TokenType::Keyword(kv) => {
                match kv {
                    Keyword::In => {}
                    kv => return Err(ParserError::UnexpectedKeywordInStream(kv.clone(), in_keyword.clone())),
                }
            }
            _ => return Err(ParserError::UnexpectedTokenInStream(in_keyword.clone()))
        };

        let expression = self.parse_expression()?;

        node.children.push(expression);


        let open_curly = self.get_next_token()?;
        Parser::validate_token_is_delimiter(open_curly, Delimiter::OpenCurlyBracket)?;

        node.children.push(self.parse_scope()?);

        Ok(node)
    }

    fn parse_expression(&mut self) -> Result<ASTNode, ParserError> {
        let next = self.peek_next_token(0)?.clone();

        let lhs;
        if Parser::token_is_delimiter(&next, Delimiter::OpenParen) {
            self.get_next_token()?;
            lhs = self.parse_expression()?;
            self.get_next_token()?;
        } else {
            lhs = self.parse_sub_expression()?;
        }

        let mut node = ASTNode::new(ASTType::Expression, next.position);

        node.children.push(lhs);

        let operator;

        let next_expression_operator = self.get_next_expression_operator()?;
        if next_expression_operator.is_some() {
            let op = next_expression_operator.unwrap();
            operator = ASTNode::new(ASTType::ExpressionOp(op.0), op.1);
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

        let result = if Parser::token_is_delimiter(delimiter, Delimiter::OpenParen) || Parser::token_is_delimiter(&delimiter, Delimiter::Colon) {
            self.parse_function_call()?
        } else if Parser::token_is_delimiter(first_delimiter, Delimiter::OpenCurlyBracket) {
            self.parse_object_declaration()?
        } else if Parser::token_is_delimiter(first_delimiter, Delimiter::OpenBracket) {
            self.parse_array_declaration()?
        } else {
            let token = self.get_next_token()?;
            Parser::token_to_simple_ast_node(&token)?
        };

        let delimiter = self.peek_next_token(0)?;
        let delimiter1 = self.peek_next_token(1);

        if Parser::token_is_delimiter(delimiter, Delimiter::Dot) &&
            (delimiter1.is_err() || !Parser::token_is_delimiter(delimiter1.unwrap(), Delimiter::Dot)) {
            let object_access = self.parse_object_access(result)?;

            return Ok(object_access);
        } else if Parser::token_is_delimiter(delimiter, Delimiter::OpenBracket) {
            let subscript = self.parse_subscript_expression(result)?;

            return Ok(subscript);
        }

        return Ok(result);
    }

    fn parse_array_declaration(&mut self) -> Result<ASTNode, ParserError> {
        let mut array_node = ASTNode::new(ASTType::CreateArray, CodeSegment::new(1, 1, 1, 1));

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
        let mut object_node = ASTNode::new(ASTType::CreateObject, CodeSegment::new(1, 1, 1, 1));

        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::OpenCurlyBracket)?;
        while !Parser::token_is_delimiter(self.peek_next_token(0)?, Delimiter::CloseCurlyBracket) {
            let token = self.get_next_token()?.clone();
            let identifier = match &token.token_type {
                TokenType::Identifier(identifier) => Ok(identifier.clone()),
                _ => Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(token.clone()))
            }?;


            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Colon)?;
            let expression = self.parse_expression()?;

            let mut object_member = ASTNode::new(ASTType::ObjectMember(identifier), token.position);

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

    fn parse_object_access(&mut self, object: ASTNode) -> Result<ASTNode, ParserError> {
        let delimiter = self.get_next_token()?;
        Parser::validate_token_is_delimiter(delimiter, Delimiter::Dot)?;

        let mut node = ASTNode::new(ASTType::ObjectAccess, delimiter.position);

        let identifier = Parser::token_to_simple_ast_node(self.get_next_token()?)?;
        node.children.push(identifier);
        node.children.push(object);

        let next_token = self.peek_next_token(0)?;
        if Parser::token_is_delimiter(next_token, Delimiter::Dot) {
            let next = self.parse_object_access(node)?;

            return Ok(next);
        } else if Parser::token_is_delimiter(next_token, Delimiter::OpenBracket) {
            let next = self.parse_subscript_expression(node)?;

            return Ok(next);
        }

        Ok(node)
    }


    fn parse_subscript_expression(&mut self, object: ASTNode) -> Result<ASTNode, ParserError> {
        let token = self.get_next_token()?;

        Parser::validate_token_is_delimiter(token, Delimiter::OpenBracket)?;
        let mut node = ASTNode::new(ASTType::Subscript, token.position);
        let result = self.parse_expression()?;
        node.children.push(result);

        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::CloseBracket)?;

        node.children.push(object);
        let next_token = self.peek_next_token(0)?;
        if Parser::token_is_delimiter(next_token, Delimiter::OpenBracket) {
            let next = self.parse_subscript_expression(node)?;

            return Ok(next);
        } else if Parser::token_is_delimiter(next_token, Delimiter::Dot) {
            let next = self.parse_object_access(node)?;

            return Ok(next);
        }


        Ok(node)

        /*
        else if Parser::token_is_delimiter(next_token, Delimiter::Dot) {
            let next = self.parse_object_access()?;

            node.children.push(next);

            return Ok(node);
        }
        */
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

        let identifier_token = self.get_next_token()?;
        let mut identifier = match &identifier_token.token_type {
            TokenType::Identifier(identifier) => identifier.clone(),
            _ => return Err(ParserError::UnexpectedTokenInStreamExpectedIdentifier(identifier_token.clone())),
        };

        identifier = namespace + &identifier;

        let mut node = ASTNode::new(ASTType::FunctionCall(identifier), identifier_token.position);

        let open_paren = self.get_next_token()?;
        Parser::validate_token_is_delimiter(open_paren, Delimiter::OpenParen)?;

        // Parameters
        loop {
            if Parser::token_is_delimiter(self.peek_next_token(0)?, Delimiter::CloseParen) {
                break;
            }

            node.children.push(self.parse_expression()?);

            let token = self.peek_next_token(0)?;
            if Parser::token_is_delimiter(token, Delimiter::Comma) {
                self.get_next_token()?;
            } else {
                break;
            }
        }

        let close_paren = self.get_next_token()?;
        Parser::validate_token_is_delimiter(close_paren, Delimiter::CloseParen)?;

        return Ok(node);
    }

    fn operator_precedence(node: &ASTNode) -> i32 {
        match &node.ast_type {
            ASTType::ExpressionOp(op) => {
                match op {
                    ExpressionOp::Assign => 0,
                    ExpressionOp::AddAssign => 0,
                    ExpressionOp::SubtractAssign => 0,
                    ExpressionOp::MultiplyAssign => 0,
                    ExpressionOp::DivideAssign => 0,

                    ExpressionOp::And => 0,
                    ExpressionOp::Or => 0,

                    ExpressionOp::Equal => 1,
                    ExpressionOp::NotEqual => 1,
                    ExpressionOp::LessThan => 1,
                    ExpressionOp::GreaterThan => 1,
                    ExpressionOp::LessThanOrEqual => 1,
                    ExpressionOp::GreaterThanOrEqual => 1,

                    ExpressionOp::Add => 1,
                    ExpressionOp::Subtract => 1,

                    ExpressionOp::Multiply => 2,
                    ExpressionOp::Divide => 2,

                    ExpressionOp::Range => 3,
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
                Ok(ASTNode::new(ASTType::Identifier(identifier.clone()), token.position))
            }
            TokenType::String(value) => {
                Ok(ASTNode::new(ASTType::StringValue(value.clone()), token.position))
            }
            TokenType::Integer(value) => {
                Ok(ASTNode::new(ASTType::IntegerValue(*value), token.position))
            }
            TokenType::Float(value) => {
                Ok(ASTNode::new(ASTType::FloatValue(*value), token.position))
            }
            _ => Err(ParserError::UnexpectedTokenInStream(token.clone()))
        }
    }

    fn combine_code_segments(segment1: CodeSegment, segment2: CodeSegment) -> CodeSegment {
        let mut smallest_x = segment1.start_x;
        if segment2.start_x < smallest_x { smallest_x = segment2.start_x }
        let mut smallest_y = segment1.start_y;
        if segment2.start_y < smallest_y { smallest_y = segment2.start_y }

        let mut largest_x = segment1.end_x;
        if segment2.end_x < largest_x { largest_x = segment2.end_x }
        let mut largest_y = segment1.end_y;
        if segment2.end_y < largest_y { largest_y = segment2.end_y }

        CodeSegment::new(smallest_x, smallest_y, largest_x, largest_y)
    }

    fn get_next_expression_operator(&mut self) -> Result<Option<(ExpressionOp, CodeSegment)>, ParserError> {
        let token = self.peek_next_token(0)?;
        let token2 = match self.peek_next_token(1) {
            Ok(t) => Some(t),
            Err(_) => None,
        };

        println!("Token 1 {:?}, Token 2 {:?}", token, token2);

        if token2.is_some() {
            if Parser::token_is_delimiter(token2.unwrap(), Delimiter::Equal) {
                if Parser::token_is_delimiter(&token, Delimiter::Equal) {
                    let token1 = self.get_next_token()?.clone();
                    let token2 = self.get_next_token()?;
                    return Ok(Some((ExpressionOp::Equal, Parser::combine_code_segments(token1.position, token2.position))));
                } else if Parser::token_is_delimiter(&token, Delimiter::Exclamation) {
                    let token1 = self.get_next_token()?.clone();
                    let token2 = self.get_next_token()?;
                    return Ok(Some((ExpressionOp::NotEqual, Parser::combine_code_segments(token1.position, token2.position))));
                } else if Parser::token_is_delimiter(&token, Delimiter::LessThan) {
                    let token1 = self.get_next_token()?.clone();
                    let token2 = self.get_next_token()?;
                    return Ok(Some((ExpressionOp::LessThanOrEqual, Parser::combine_code_segments(token1.position, token2.position))));
                } else if Parser::token_is_delimiter(&token, Delimiter::GreaterThan) {
                    let token1 = self.get_next_token()?.clone();
                    let token2 = self.get_next_token()?;
                    return Ok(Some((ExpressionOp::GreaterThanOrEqual, Parser::combine_code_segments(token1.position, token2.position))));
                } else if Parser::token_is_delimiter(&token, Delimiter::Plus) {
                    let token1 = self.get_next_token()?.clone();
                    let token2 = self.get_next_token()?;
                    return Ok(Some((ExpressionOp::AddAssign, Parser::combine_code_segments(token1.position, token2.position))));
                } else if Parser::token_is_delimiter(&token, Delimiter::Hyphen) {
                    let token1 = self.get_next_token()?.clone();
                    let token2 = self.get_next_token()?;
                    return Ok(Some((ExpressionOp::SubtractAssign, Parser::combine_code_segments(token1.position, token2.position))));
                } else if Parser::token_is_delimiter(&token, Delimiter::Star) {
                    let token1 = self.get_next_token()?.clone();
                    let token2 = self.get_next_token()?;
                    return Ok(Some((ExpressionOp::MultiplyAssign, Parser::combine_code_segments(token1.position, token2.position))));
                } else if Parser::token_is_delimiter(&token, Delimiter::Slash) {
                    let token1 = self.get_next_token()?.clone();
                    let token2 = self.get_next_token()?;
                    return Ok(Some((ExpressionOp::DivideAssign, Parser::combine_code_segments(token1.position, token2.position))));
                }
            } else if Parser::token_is_delimiter(&token, Delimiter::And) && Parser::token_is_delimiter(&token2.unwrap(), Delimiter::And) {
                let token1 = self.get_next_token()?.clone();
                let token2 = self.get_next_token()?;
                return Ok(Some((ExpressionOp::And, Parser::combine_code_segments(token1.position, token2.position))));
            } else if Parser::token_is_delimiter(&token, Delimiter::Pipe) && Parser::token_is_delimiter(&token2.unwrap(), Delimiter::Pipe) {
                let token1 = self.get_next_token()?.clone();
                let token2 = self.get_next_token()?;
                return Ok(Some((ExpressionOp::Or, Parser::combine_code_segments(token1.position, token2.position))));
            } else if Parser::token_is_delimiter(&token, Delimiter::Dot) && Parser::token_is_delimiter(&token2.unwrap(), Delimiter::Dot) {
                let token1 = self.get_next_token()?.clone();
                let token2 = self.get_next_token()?;
                println!("Range expression");
                return Ok(Some((ExpressionOp::Range, Parser::combine_code_segments(token1.position, token2.position))));
            }
        }

        if Parser::token_is_delimiter(&token, Delimiter::LessThan) {
            let token1 = self.get_next_token()?;
            return Ok(Some((ExpressionOp::LessThan, token1.position)));
        } else if Parser::token_is_delimiter(&token, Delimiter::GreaterThan) {
            let token1 = self.get_next_token()?;
            return Ok(Some((ExpressionOp::GreaterThan, token1.position)));
        } else if Parser::token_is_delimiter(&token, Delimiter::Plus) {
            let token1 = self.get_next_token()?;
            return Ok(Some((ExpressionOp::Add, token1.position)));
        } else if Parser::token_is_delimiter(&token, Delimiter::Hyphen) {
            let token1 = self.get_next_token()?;
            return Ok(Some((ExpressionOp::Subtract, token1.position)));
        } else if Parser::token_is_delimiter(&token, Delimiter::Star) {
            let token1 = self.get_next_token()?;
            return Ok(Some((ExpressionOp::Multiply, token1.position)));
        } else if Parser::token_is_delimiter(&token, Delimiter::Slash) {
            let token1 = self.get_next_token()?;
            return Ok(Some((ExpressionOp::Divide, token1.position)));
        } else if Parser::token_is_delimiter(&token, Delimiter::Equal) {
            let token1 = self.get_next_token()?;
            return Ok(Some((ExpressionOp::Assign, token1.position)));
        }


        return Ok(None);
    }

    fn validate_token_is_delimiter(token: &Token, delimiter: Delimiter) -> Result<(), ParserError> {
        match Parser::token_is_delimiter(token, delimiter.clone()) {
            true => Ok({}),
            false => match &token.token_type {
                TokenType::Delimiter(_) => {
                    Err(ParserError::UnexpectedDelimiterInStream(delimiter, token.clone()))
                }
                _ => {
                    Err(ParserError::UnexpectedTokenInStreamWithExpected(TokenType::Delimiter(delimiter), token.clone()))
                }
            }
        }
    }
}