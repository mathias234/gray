use std::fs::File;
use std::io::{Read, ErrorKind};
use crate::bytecode::code_block::CodeSegment;
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Delimiter {
    Space,
    OpenBracket,
    CloseBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,
    OpenParen,
    CloseParen,
    Comma,
    Dot,
    Semicolon,
    Colon,
    Hyphen,
    Star,
    Plus,
    Slash,
    Equal,
    And,
    Pipe,
    LessThan,
    GreaterThan,
    Exclamation,
    Quotation,
    LineFeed,
    CarriageReturn,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Keyword {
    Public,
    Private,
    Function,
    VariableDeclaration,
    IfStatement,
    ElseStatement,
    WhileStatement,
    Return,
    Namespace,
    Break,
    Continue,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenType {
    Identifier(String),
    Integer(i64),
    Float(f64),
    String(String),
    Delimiter(Delimiter),
    Keyword(Keyword),
    EndOfFile,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub position: CodeSegment,

}

impl Token {
    pub fn new(token_type: TokenType, position: CodeSegment) -> Token {
        Token {
            token_type,
            position,
        }
    }
}

impl PartialEq<TokenType> for Token {
    fn eq(&self, other: &TokenType) -> bool {
        self.token_type == *other
    }
}

pub struct TokenStream {
    tokens: Vec<Token>,
    position: usize,
}

impl TokenStream {
    pub fn new() -> TokenStream {
        TokenStream {
            tokens: Vec::new(),
            position: 0,
        }
    }

    pub fn reset(&mut self) {
        self.position = 0;
    }

    pub fn next(&mut self) -> Option<&Token> {
        if self.position >= self.tokens.len() {
            return None;
        }

        let result = Some(&self.tokens[self.position]);
        self.position += 1;
        return result;
    }

    pub fn peek_next(&self, offset: usize) -> Option<&Token> {
        if self.position + offset >= self.tokens.len() {
            return None;
        }

        let result = Some(&self.tokens[self.position + offset]);
        return result;
    }
}


#[derive(Debug)]
pub enum LexerError {
    FileNotFound,
    UnexpectedTokenWhileParsingFloat(Token),
    UnknownErrorParsingFloat,
    UnknownError,
}

pub struct Lexer {
    file: String,
    position: usize,
    pos_x: usize,
    pos_y: usize,
}

impl Lexer {
    pub fn lex_string(string: &str) -> Result<TokenStream, LexerError> {
        let mut token_stream = TokenStream::new();

        let mut lexer = Lexer {
            file: string.to_string(),
            position: 0,
            pos_x: 1,
            pos_y: 1,
        };

        loop {
            let token = lexer.pop_token()?;

            if token == TokenType::EndOfFile {
                return Ok(token_stream);
            }

            token_stream.tokens.push(token);
        }
    }


    pub fn lex_file(file: &str) -> Result<(Rc<String>, TokenStream), LexerError> {
        let file_result = File::open(file);
        let mut file = String::new();

        match file_result {
            Err(e) => return match e.kind() {
                ErrorKind::NotFound => Err(LexerError::FileNotFound),
                _ => Err(LexerError::UnknownError),
            },
            Ok(mut f) => match f.read_to_string(&mut file) {
                Ok(_) => {}
                Err(e) => return match e.kind() {
                    _ => Err(LexerError::UnknownError),
                },
            },
        };


        let mut lexer = Lexer {
            file: file.clone(),
            position: 0,
            pos_x: 1,
            pos_y: 1,
        };

        let mut token_stream = TokenStream::new();

        loop {
            let token = lexer.pop_token()?;

            if token == TokenType::EndOfFile {
                while let Some(token) = token_stream.next() {
                    println!("{:?}", token);
                }

                token_stream.reset();

                return Ok((Rc::from(file), token_stream));
            }

            token_stream.tokens.push(token);
        }
    }

    pub fn pop_token(&mut self) -> Result<Token, LexerError> {
        let mut word = String::new();
        let chars = &mut self.file[self.position..self.file.len()].chars();

        let mut is_string = false;
        let mut is_comment = false;
        let mut saw_hyphen = false;

        let start_x = self.pos_x;
        let start_y = self.pos_y;

        loop {
            let char = chars.next();


            if char.is_none() {
                return Ok(Token::new(TokenType::EndOfFile, CodeSegment::new(start_x, start_y, self.pos_x, self.pos_y)));
            }

            let char = char.unwrap();

            let delimiter = Lexer::char_to_delimiter(char);
            if delimiter.is_some() {
                let delimiter = delimiter.unwrap();
                if delimiter == Delimiter::Quotation {
                    self.position += char.len_utf8();
                    self.pos_x += 1;

                    if is_string {
                        break;
                    }

                    is_string = true;
                    continue;
                } else if delimiter == Delimiter::Slash && !is_string {
                    if let Some(delimiter) = chars.next_back() {
                        if let Some(delimiter) = Lexer::char_to_delimiter(delimiter) {
                            if delimiter == Delimiter::Slash {
                                chars.next();
                                is_comment = true;
                                self.position += char.len_utf8();
                                self.pos_x += 1;
                                continue;
                            }
                        }
                    }
                }

                if is_comment {
                    self.position += char.len_utf8();
                    match delimiter {
                        Delimiter::LineFeed => {
                            return self.pop_token();
                        }
                        _ => {}
                    }
                    continue;
                }

                // While parsing strings any character is fine
                if is_string {
                    word += &String::from(char);
                    self.position += char.len_utf8();
                    self.pos_x += 1;
                    continue;
                }

                if saw_hyphen == false && delimiter == Delimiter::Hyphen {
                    saw_hyphen = true;
                } else if delimiter == Delimiter::Dot && Lexer::word_to_integer(&word).is_some() {
                    // Seems like a float, as we have a full integer before then a dot
                    // Lets try and pop another token to get the fractional portion

                    self.position += char.len_utf8();

                    let integer = Lexer::word_to_integer(&word).unwrap();
                    let fractional = self.pop_token()?;

                    return match fractional.token_type {
                        TokenType::Integer(i) => {
                            let result = Lexer::integer_and_fractional_to_float(integer, i)?;
                            Ok(Token::new(TokenType::Float(result), CodeSegment::new(start_x, start_y, self.pos_x, self.pos_y)))
                        }
                        _ => Err(LexerError::UnexpectedTokenWhileParsingFloat(fractional))
                    };
                } else if word.len() > 0 {
                    break;
                } else {
                    self.position += char.len_utf8();
                    self.pos_x += 1;

                    // Space and new line is ignored
                    match delimiter {
                        Delimiter::Space => return self.pop_token(),
                        Delimiter::LineFeed => {
                            self.pos_y += 1;
                            self.pos_x = 1;

                            return self.pop_token();
                        }
                        Delimiter::CarriageReturn => return self.pop_token(),
                        _ => {}
                    }

                    return Ok(Token::new(TokenType::Delimiter(delimiter), CodeSegment::new(start_x, start_y, self.pos_x, self.pos_y)));
                }
            }

            self.position += char.len_utf8();
            self.pos_x += 1;

            if !is_comment {
                word += &String::from(char);
            }
        }


        if is_string {
            return Ok(Token::new(TokenType::String(word), CodeSegment::new(start_x, start_y, self.pos_x, self.pos_y)));
        }

        match Lexer::word_to_delimiter(&word) {
            Some(value) => return Ok(Token::new(TokenType::Delimiter(value), CodeSegment::new(start_x, start_y, self.pos_x, self.pos_y))),
            None => {}
        }

        match Lexer::word_to_integer(&word) {
            Some(value) => return Ok(Token::new(TokenType::Integer(value), CodeSegment::new(start_x, start_y, self.pos_x, self.pos_y))),
            None => {}
        };


        match Lexer::word_to_keyword(&word) {
            Some(keyword) => return Ok(Token::new(TokenType::Keyword(keyword), CodeSegment::new(start_x, start_y, self.pos_x, self.pos_y))),
            None => {}
        }


        return Ok(Token::new(TokenType::Identifier(word), CodeSegment::new(start_x, start_y, self.pos_x, self.pos_y)));
    }

    fn word_to_integer(word: &str) -> Option<i64> {
        // TODO: Parse as hex
        if word.starts_with("0x") {}

        let value: i64 = match word.parse() {
            Ok(v) => v,
            Err(_) => return None,
        };

        Some(value)
    }

    fn word_to_delimiter(word: &str) -> Option<Delimiter> {
        if word.len() > 1 {
            return None;
        }

        let value = Lexer::char_to_delimiter(word.chars().nth(0).unwrap());

        return value;
    }

    fn integer_and_fractional_to_float(integer: i64, fractional: i64) -> Result<f64, LexerError> {
        // FIXME: This do be hacky
        let float_as_string = format!("{}.{}", integer, fractional);

        let result = float_as_string.parse::<f64>();
        match result {
            Ok(v) => Ok(v),
            Err(_) => Err(LexerError::UnknownErrorParsingFloat),
        }
    }

    fn word_to_keyword(word: &str) -> Option<Keyword> {
        match word {
            "public" => Some(Keyword::Public),
            "private" => Some(Keyword::Private),
            "fn" => Some(Keyword::Function),
            "let" => Some(Keyword::VariableDeclaration),
            "if" => Some(Keyword::IfStatement),
            "else" => Some(Keyword::ElseStatement),
            "while" => Some(Keyword::WhileStatement),
            "return" => Some(Keyword::Return),
            "namespace" => Some(Keyword::Namespace),
            "break" => Some(Keyword::Break),
            "continue" => Some(Keyword::Continue),
            _ => None,
        }
    }

    fn char_to_delimiter(token_char: char) -> Option<Delimiter> {
        match token_char {
            ' ' => Some(Delimiter::Space),
            '[' => Some(Delimiter::OpenBracket),
            ']' => Some(Delimiter::CloseBracket),
            '{' => Some(Delimiter::OpenCurlyBracket),
            '}' => Some(Delimiter::CloseCurlyBracket),
            '(' => Some(Delimiter::OpenParen),
            ')' => Some(Delimiter::CloseParen),
            ',' => Some(Delimiter::Comma),
            '.' => Some(Delimiter::Dot),
            ';' => Some(Delimiter::Semicolon),
            ':' => Some(Delimiter::Colon),
            '-' => Some(Delimiter::Hyphen),
            '*' => Some(Delimiter::Star),
            '+' => Some(Delimiter::Plus),
            '/' => Some(Delimiter::Slash),
            '=' => Some(Delimiter::Equal),
            '&' => Some(Delimiter::And),
            '|' => Some(Delimiter::Pipe),
            '<' => Some(Delimiter::LessThan),
            '>' => Some(Delimiter::GreaterThan),
            '!' => Some(Delimiter::Exclamation),
            '\"' => Some(Delimiter::Quotation),
            '\n' => Some(Delimiter::LineFeed),
            '\r' => Some(Delimiter::CarriageReturn),
            _ => None,
        }
    }
}