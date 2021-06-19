use std::fs::File;
use std::io::{Read, ErrorKind};

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
    Slash,
    Equal,
    LineFeed,
    CarriageReturn,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Keyword {
    Public,
    Private,
    Function,
    Structure,
    Trait,
    VariableDeclaration,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Identifier(String),
    Integer(i128),
    Float(f64),
    Delimiter(Delimiter),
    Keyword(Keyword),
    EndOfFile,
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
}

impl Lexer {
    pub fn lex_file(file: &str) -> Result<TokenStream, LexerError> {
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
            file,
            position: 0,
        };

        let mut token_stream = TokenStream::new();

        loop {
            let token = lexer.pop_token()?;

            if token == Token::EndOfFile {
                return Ok(token_stream);
            }

            token_stream.tokens.push(token);
        }
    }

    pub fn pop_token(&mut self) -> Result<Token, LexerError> {
        let mut word = String::new();
        let chars = &mut self.file[self.position..self.file.len()].chars();

        loop {
            let char = chars.next();

            if char.is_none() {
                return Ok(Token::EndOfFile);
            }

            let char = char.unwrap();

            let delimiter = Lexer::char_to_delimiter(char);
            if delimiter.is_some() {
                let delimiter = delimiter.unwrap();
                if delimiter == Delimiter::Dot && Lexer::word_to_integer(&word).is_some() {
                    // Seems like a float, as we have a full integer before then a dot
                    // Lets try and pop another token to get the fractional portion

                    self.position += char.len_utf8();

                    let integer = Lexer::word_to_integer(&word).unwrap();
                    let fractional = self.pop_token()?;

                    return match fractional {
                        Token::Integer(i) => {
                            let result = Lexer::integer_and_fractional_to_float(integer, i)?;
                            Ok(Token::Float(result))
                        }
                        _ => Err(LexerError::UnexpectedTokenWhileParsingFloat(fractional))
                    };
                } else if word.len() > 0 {
                    break;
                } else {
                    self.position += char.len_utf8();

                    // Space and new line is ignored
                    match delimiter {
                        Delimiter::Space => return self.pop_token(),
                        Delimiter::LineFeed => return self.pop_token(),
                        Delimiter::CarriageReturn => return self.pop_token(),
                        _ => {}
                    }

                    return Ok(Token::Delimiter(delimiter));
                }
            }

            self.position += char.len_utf8();

            word += &String::from(char);
        }

        match Lexer::word_to_integer(&word) {
            Some(value) => return Ok(Token::Integer(value)),
            None => {}
        };


        match Lexer::word_to_keyword(&word) {
            Some(keyword) => return Ok(Token::Keyword(keyword)),
            None => {}
        }


        return Ok(Token::Identifier(word));
    }

    fn word_to_integer(word: &str) -> Option<i128> {
        // TODO: Parse as hex
        if word.starts_with("0x") {}

        let value: i128 = match word.parse() {
            Ok(v) => v,
            Err(_) => return None,
        };

        Some(value)
    }

    fn integer_and_fractional_to_float(integer: i128, fractional: i128) -> Result<f64, LexerError> {
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
            "trait" => Some(Keyword::Trait),
            "struct" => Some(Keyword::Structure),
            "let" => Some(Keyword::VariableDeclaration),
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
            '/' => Some(Delimiter::Slash),
            '=' => Some(Delimiter::Equal),
            '\n' => Some(Delimiter::LineFeed),
            '\r' => Some(Delimiter::CarriageReturn),
            _ => None,
        }
    }
}