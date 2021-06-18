use crate::parser::lexer::{TokenStream, Token, Keyword};

#[derive(PartialEq, Debug)]
pub enum ASTType {
    ProgramRoot,
    Scope,
    Structure,
    Trait(String)
}

#[derive(Debug)]
pub enum ParserError {
    UnexpectedEndOfProgram,
    UnexpectedKeywordInStream(Keyword),
    UnexpectedTokenInStream(Token),
}

#[derive(Debug)]
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

        let mut root = ASTNode::new(ASTType::ProgramRoot);
        root.children.push(parser.parse_scope()?);

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
                        _ => Err(ParserError::UnexpectedKeywordInStream(keyword.clone())),
                    }
                }
                _ => Err(ParserError::UnexpectedTokenInStream(token.clone())),
            };

            scope.children.push(child?);
        }
    }

    fn parse_trait(&mut self) -> Result<ASTNode, ParserError> {
        let name_token = self.get_next_token()?;

        match name_token {
            Token::Identifier(name) => {},
            _ => return Err(ParserError::UnexpectedTokenInStream(name_token.clone())),
        }

        Ok(ASTNode::new(ASTType::Structure))
    }

    fn parse_structure(&mut self) -> Result<ASTNode, ParserError> {
        Ok(ASTNode::new(ASTType::Structure))
    }

    fn get_next_token(&mut self) -> Result<&Token, ParserError> {
        let token = self.token_stream.next();
        match token {
            Some(token) => Ok(token),
            None => Err(ParserError::UnexpectedEndOfProgram)
        }
    }
}