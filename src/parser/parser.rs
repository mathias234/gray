use crate::parser::lexer::{TokenStream, Token, Keyword};
use crate::parser::lexer::Delimiter;


#[derive(PartialEq, Debug, Clone)]
pub enum ASTType {
    ProgramRoot,
    Scope,
    Expression,
    Function(String),
    Structure(String),
    FunctionCall(String),
    VariableDeclaration(String),
    Trait(String),
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEndOfProgram,
    UnexpectedKeywordInStream(Keyword),
    UnexpectedDelimiterInStream(Delimiter),
    UnexpectedTokenInStream(Token),

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
                        _ => Err(ParserError::UnexpectedDelimiterInStream(d.clone())),
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

            let delimiter = self.get_next_token()?;
            Parser::validate_token_is_delimiter(delimiter, Delimiter::CloseParen)?;
            let delimiter = self.get_next_token()?;
            Parser::validate_token_is_delimiter(delimiter, Delimiter::Semicolon)?;

            return Ok(ASTNode::new(ASTType::FunctionCall(built_identifier)));
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
        // Parse parameters

        let token = self.get_next_token()?;
        Parser::validate_token_is_delimiter(token, Delimiter::CloseParen)?;


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


        Ok(node)
    }

    fn parse_expression(&mut self) -> Result<ASTNode, ParserError> {
        let node = ASTNode::new(ASTType::Expression);
        Ok(node)
    }


    fn get_next_token(&mut self) -> Result<&Token, ParserError> {
        let token = self.token_stream.next();
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

    fn validate_token_is_delimiter(token: &Token, delimiter: Delimiter) -> Result<(), ParserError> {
        return match Parser::token_is_delimiter(token, delimiter) {
            true => Ok({}),
            false => Err(ParserError::UnexpectedTokenInStream(token.clone())),
        };
    }
}