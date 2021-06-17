use std::fs::File;
use std::io::{Read, ErrorKind};

#[derive(PartialEq, Debug)]
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
    Slash,
    LineFeed,
    CarriageReturn,
}

#[derive(PartialEq, Debug)]
pub enum Token {
    Identifier(String),
    Delimiter(Delimiter),
    EndOfFile,
}

pub struct TokenStream {
    pub tokens: Vec<Token>,
}

impl TokenStream {
    pub fn new() -> TokenStream {
        TokenStream {
            tokens: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum LexerError {
    FileNotFound,
    UnknownError,
}

pub struct Lexer {
    file: String,
    position: usize,
}

impl Lexer {
    pub fn lex_file(file: &str) -> Result<TokenStream, LexerError> {
        let file_result = File::open(file);
        let mut file = String::new();

        if file_result.is_err() {
            return match file_result.unwrap_err().kind() {
                ErrorKind::NotFound => Err(LexerError::FileNotFound),
                _ => Err(LexerError::UnknownError),
            };
        } else if file_result.is_ok() {
            file_result.expect("").read_to_string(&mut file);
        }

        let mut lexer = Lexer {
            file,
            position: 0,
        };

        let mut token_stream = TokenStream::new();

        loop {
            let token = lexer.pop_token();

            if token == Token::EndOfFile {
                return Ok(token_stream);
            }

            token_stream.tokens.push(token);
        }
    }

    pub fn pop_token(&mut self) -> Token {
        let mut word = String::new();
        let mut chars = &mut self.file[self.position..self.file.len()].chars();

        loop {
            let char = chars.next();

            if char.is_none() {
                return Token::EndOfFile;
            }

            let char = char.unwrap();

            let delimiter = Lexer::char_to_delimiter(char);
            if delimiter.is_some() {
                // We have started a word, so we just break without incrementing
                // This assures that we get the delimiter on the next pop_token call
                // without adding it to the current word
                if word.len() > 0 {
                    break;
                }
                else {
                    self.position += char.len_utf8();
                    return Token::Delimiter(delimiter.unwrap());
                }
            }

            self.position += char.len_utf8();

            word += &String::from(char);
        }

        return Token::Identifier(word);
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
            '/' => Some(Delimiter::Slash),
            '\n' => Some(Delimiter::LineFeed),
            '\r' => Some(Delimiter::CarriageReturn),
            _ => None,
        }
    }
}