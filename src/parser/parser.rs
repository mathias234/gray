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
    ReturnExpression,
    IfStatement,
    WhileStatement,
    ComparisonOp(ComparisonOp),
    MathOp(MathOp),
    Function(String),
    Structure(String),
    FunctionCall(String),
    VariableDeclaration(String),
    VariableAssignment,
    FloatValue(f64),
    IntegerValue(i64),
    StringValue(String),
    Identifier(String),
    CreateObject,
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
            let token_result = self.peek_next_token(0);

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
                        Keyword::Return => {
                            self.get_next_token()?;
                            let mut return_node = ASTNode::new(ASTType::ReturnExpression);
                            return_node.children.push(self.parse_expression()?);
                            Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::Semicolon)?;
                            Ok(return_node)
                        }
                        _ => Err(ParserError::UnexpectedKeywordInStream(keyword.clone())),
                    }
                }
                Token::Delimiter(d) => {
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

    fn parse_trait(&mut self) -> Result<ASTNode, ParserError> {
        self.get_next_token()?;

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
        self.get_next_token()?;

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
        self.get_next_token()?;

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
        self.get_next_token()?;

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
        let mut node;

        node = self.parse_non_math_expression()?;

        let next = self.peek_next_token(0)?;
        let next1 = self.peek_next_token(1)?;

        if Parser::token_is_math_delimiter(next) {
            let math_expr = self.parse_math_expression(node)?;

            node = ASTNode::new(ASTType::Expression);
            node.children.push(math_expr);
        } else if Parser::tokens_are_comparison(next, next1).is_some() {
            let comp_expr = self.parse_comparison_expression(node)?;

            node = ASTNode::new(ASTType::Expression);
            node.children.push(comp_expr);
        }

        Ok(node)
    }

    fn parse_non_math_expression(&mut self) -> Result<ASTNode, ParserError> {
        let mut node = ASTNode::new(ASTType::Expression);

        let first_delimiter = self.peek_next_token(0)?;
        let delimiter = self.peek_next_token(1)?;

        if Parser::token_is_delimiter(&delimiter, Delimiter::OpenParen) {
            node.children.push(self.parse_function_call()?);
        } else if Parser::token_is_delimiter(&first_delimiter, Delimiter::OpenCurlyBracket) {
            node.children.push(self.parse_object_declaration()?);
        } else {
            // Very simple single token expression
            let token = self.get_next_token()?;
            node.children.push(Parser::token_to_simple_ast_node(&token)?);
        }

        Ok(node)
    }

    fn parse_object_declaration(&mut self) -> Result<ASTNode, ParserError> {
        let mut object_node = ASTNode::new(ASTType::CreateObject);

        Parser::validate_token_is_delimiter(self.get_next_token()?, Delimiter::OpenCurlyBracket)?;
        while !Parser::token_is_delimiter(self.peek_next_token(0)?, Delimiter::CloseCurlyBracket) {
            let identifier = match self.get_next_token()? {
                Token::Identifier(identifier) => Ok(identifier.clone()),
                t => Err(ParserError::UnexpectedTokenInStream(t.clone()))
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
        let name = match name {
            Token::Identifier(n) => Ok(n.clone()),
            _ => Err(ParserError::UnexpectedTokenInStream(name.clone()))
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
        let identifier = self.get_next_token()?;

        let identifier = match identifier {
            Token::Identifier(identifier) => identifier.clone(),
            _ => return Err(ParserError::UnexpectedTokenInStream(identifier.clone())),
        };

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
            ASTType::MathOp(op) => {
                match op {
                    MathOp::Add => 0,
                    MathOp::Subtract => 0,
                    MathOp::Multiply => 1,
                    MathOp::Divide => 1,
                }
            }
            _ => 0,
        }
    }

    fn parse_math_expression(&mut self, lhs: ASTNode) -> Result<ASTNode, ParserError> {
        let mut node = ASTNode::new(ASTType::MathExpression);

        let operator = Parser::token_to_math_op_ast_node(self.get_next_token()?)?;

        let mut protected_expression = false;
        if Parser::token_is_delimiter(self.peek_next_token(0)?, Delimiter::OpenParen) {
            self.get_next_token()?;
            protected_expression = true;
        }

        let rhs = self.parse_expression()?;

        node.children.push(lhs);
        node.children.push(operator.clone());
        node.children.push(rhs);

        if protected_expression {
            self.get_next_token()?;

            // This is a protected expression meaning it's wrapped in ()
            // We can return early so not to do operator precedence on this
            return Ok(node);
        }

        let my_precedence = Parser::operator_precedence(&operator);

        match &node.children[2].children[0].ast_type {
            ASTType::MathExpression => {
                let child_precedence = Parser::operator_precedence(&node.children[2].children[0]);
                if child_precedence < my_precedence {
                    // Kinda messy because of all the .children[]
                    // but essentially we are just rotating the tree to the left

                    let old_operator = node.children[1].clone();
                    node.children[1] = node.children[2].children[0].children[1].clone();
                    node.children[2].children[0].children[1] = old_operator;

                    let old_lhs = node.children[0].clone();
                    let child_old_lhs = node.children[2].children[0].children[0].clone();
                    let child_old_rhs = node.children[2].children[0].children[2].clone();

                    node.children[0] = node.children[2].clone();
                    node.children[0].children[0].children[0] = old_lhs;
                    node.children[0].children[0].children[2] = child_old_lhs;
                    node.children[2] = child_old_rhs;
                }
            }
            _ => {}
        }

        Ok(node)
    }

    fn parse_comparison_expression(&mut self, lhs: ASTNode) -> Result<ASTNode, ParserError> {
        let mut node = ASTNode::new(ASTType::ComparisonExpression);

        node.children.push(lhs);

        let token = self.peek_next_token(0)?;
        let token1 = self.peek_next_token(1)?;

        let operator;

        let rhs;

        let tokens_are_comparison = Parser::tokens_are_comparison(token, token1);

        match tokens_are_comparison {
            Some(op) => operator = op,
            None => return Err(ParserError::UnexpectedTokenInStream(token.clone()))
        }

        if operator == ComparisonOp::LessThan || operator == ComparisonOp::GreaterThan {
            self.get_next_token()?;
            rhs = self.parse_expression()?;
        } else {
            self.get_next_token()?;
            self.get_next_token()?;
            rhs = self.parse_expression()?;
        }

        node.children.push(ASTNode::new(ASTType::ComparisonOp(operator)));
        node.children.push(rhs);

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
            Token::String(value) => {
                Ok(ASTNode::new(ASTType::StringValue(value.clone())))
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

    fn tokens_are_comparison(token: &Token, token2: &Token) -> Option<ComparisonOp> {
        if Parser::token_is_delimiter(token2, Delimiter::Equal) {
            if Parser::token_is_delimiter(&token, Delimiter::Equal) {
                return Some(ComparisonOp::Equal);
            } else if Parser::token_is_delimiter(&token, Delimiter::Exclamation) {
                return Some(ComparisonOp::NotEqual);
            } else if Parser::token_is_delimiter(&token, Delimiter::LessThan) {
                return Some(ComparisonOp::LessThanOrEqual);
            } else if Parser::token_is_delimiter(&token, Delimiter::GreaterThan) {
                return Some(ComparisonOp::GreaterThanOrEqual);
            }
        } else if Parser::token_is_delimiter(&token, Delimiter::LessThan) {
            return Some(ComparisonOp::LessThan);
        } else if Parser::token_is_delimiter(&token, Delimiter::GreaterThan) {
            return Some(ComparisonOp::GreaterThan);
        }

        return None;
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