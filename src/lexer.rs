use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub struct Span {
    col: u64,
    row: u64,
    length: u64,
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:>4}:{:<4}", self.row, self.col)
    }
}

pub enum Lexeme {
    Identifier(Span, String),
    Symbol(Span, Symbol),
    Keyword(Span, Keyword),
    Number(Span, String),
    String(Span, String),
    Newline(Span),
    Eof(Span),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Keyword {
    If,
    Then,
    Else,
    Given,
    True,
    False,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            Keyword::If => "if",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::Given => "given",
            Keyword::True => "true",
            Keyword::False => "false"
        })
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Symbol {
    Plus,
    Dash,
    Bang,
    Asterisk,
    Comma,
    ForwardSlash,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    DoubleAmpersand,
    DoubleBar,
    LeftArrow,
    RightArrow,
    LeftArrowEquals,
    RightArrowEquals,
    EqualsRightArrow,
    DoubleEquals,
    BangEquals,
    Equals,
    Dot,
    Colon,
    DoubleColon
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            Symbol::Plus => "+",
            Symbol::Dash => "-",
            Symbol::Asterisk => "*",
            Symbol::ForwardSlash => "/",
            Symbol::Comma => ",",
            Symbol::Bang => "!",
            Symbol::OpenBrace => "{",
            Symbol::CloseBrace => "}",
            Symbol::OpenBracket => "(",
            Symbol::CloseBracket => ")",
            Symbol::DoubleAmpersand => "&&",
            Symbol::DoubleBar => "||",
            Symbol::LeftArrow => "<",
            Symbol::RightArrow => ">",
            Symbol::LeftArrowEquals => "<=",
            Symbol::RightArrowEquals => ">=",
            Symbol::EqualsRightArrow => "=>",
            Symbol::DoubleEquals => "==",
            Symbol::BangEquals => "!=",
            Symbol::Equals => "=",
            Symbol::Dot => ".",
            Symbol::DoubleColon => "::",
            Symbol::Colon => ":"
        })
    }
}

impl Display for Lexeme {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Lexeme::Eof(pos) => write!(f, "{} EOF", pos),
            Lexeme::Newline(pos) => write!(f, "{} LINE", pos),
            Lexeme::Identifier(pos, id) => write!(f, "{} ID   {}", pos, id),
            Lexeme::Symbol(pos, sym) => write!(f, "{} SYM  {}", pos, sym),
            Lexeme::Number(pos, id) => write!(f, "{} NUM  {}", pos, id),
            Lexeme::String(pos, id) => write!(f, "{} STR  {}", pos, id),
            Lexeme::Keyword(pos, keyword) => write!(f, "{} KEY  {}", pos, keyword)
        }
    }
}

fn build_identifier(col: u64, row: u64, word: &String, is_number: bool, is_string: bool) -> Lexeme {
    let word_length = word.len() as u64;
    let pos = Span { col: col - word_length as u64, row, length: word_length };
    if is_number {
        Lexeme::Number(pos, word.clone())
    } else if is_string {
        Lexeme::String(pos, word.clone())
    } else {
        match word.as_str() {
            "if" => Lexeme::Keyword(pos, Keyword::If),
            "then" => Lexeme::Keyword(pos, Keyword::Then),
            "else" => Lexeme::Keyword(pos, Keyword::Else),
            "given" => Lexeme::Keyword(pos, Keyword::Given),
            "true" => Lexeme::Keyword(pos, Keyword::True),
            "false" => Lexeme::Keyword(pos, Keyword::False),
            _ => Lexeme::Identifier(pos, word.clone())
        }
    }
}

pub fn lex(code: String) -> Result<Vec<Lexeme>, Box<dyn Error>> {
    let mut vec = Vec::new();
    let mut col: u64 = 0;
    let mut row: u64 = 0;
    let mut word: String = "".to_string();
    let mut is_number = false;
    let mut is_decimal = false;
    let mut skip = false;
    let mut is_string = false;

    for (i, c) in code.chars().enumerate() {
        if skip {
            skip = false;
        } else {
            match c {
                '\r' => (),
                '\n' if !is_string => {
                    if !word.is_empty() {
                        vec.push(build_identifier(col, row, &word, is_number, is_string));
                    }
                    match vec.last() {
                        None => (),
                        Some(last) => {
                            if !matches!(last, Lexeme::Newline(_)) {
                                vec.push(Lexeme::Newline(Span { col, row, length: 1 }));
                            }
                        }
                    }
                    is_number = false;
                    is_decimal = false;
                    word = "".to_string();
                    col = 0;
                    row += 1;
                }
                ' '  if !is_string => {
                    if !word.is_empty() {
                        vec.push(build_identifier(col, row, &word, is_number, is_string));
                        word = "".to_string();
                    }
                    is_number = false;
                    is_decimal = false;
                    col += 1;
                }
                '.' if !is_string && is_number && !is_decimal => {
                    word.push(c);
                    is_decimal = true;
                    col += 1;
                }
                '"' => {
                    if !is_string {
                        if !word.is_empty() {
                            vec.push(build_identifier(col, row, &word, is_number, is_string));
                            word = "".to_string();
                        }
                        is_number = false;
                        is_decimal = false;
                        is_string = true;
                    } else {
                        vec.push(build_identifier(col, row, &word, is_number, is_string));
                        word = "".to_string();
                        is_string = false;
                    }
                }
                '{' | '}' | '(' | ')' | '[' | ']' | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | ':' | '|' | '&' | ',' | '.' | ';' if !is_string => {
                    if !word.is_empty() {
                        vec.push(build_identifier(col, row, &word, is_number, is_string));
                        word = "".to_string();
                    }
                    is_number = false;
                    is_decimal = false;

                    let mut symbol = c.to_string();
                    if ['+', '-', '*', '/', '=', '<', '>', '!', ':', '|', '&'].contains(&c) {
                        match code.chars().nth(i + 1) {
                            Some(char) => if ['+', '-', '*', '/', '=', '<', '>', '!', ':', '|', '&'].contains(&char) {
                                symbol.push(char);
                                skip = true;
                            }
                            None => ()
                        }
                    }

                    vec.push(Lexeme::Symbol(Span { col, row, length: symbol.len() as u64 }, match symbol.as_str() {
                        "{" => Symbol::OpenBrace,
                        "}" => Symbol::CloseBrace,
                        "(" => Symbol::OpenBracket,
                        ")" => Symbol::CloseBracket,
                        "+" => Symbol::Plus,
                        "-" => Symbol::Dash,
                        "*" => Symbol::Asterisk,
                        "/" => Symbol::ForwardSlash,
                        "," => Symbol::Comma,
                        "!" => Symbol::Bang,
                        "&&" => Symbol::DoubleAmpersand,
                        "||" => Symbol::DoubleBar,
                        "<" => Symbol::LeftArrow,
                        ">" => Symbol::RightArrow,
                        "<=" => Symbol::LeftArrowEquals,
                        ">=" => Symbol::RightArrowEquals,
                        "=>" => Symbol::EqualsRightArrow,
                        "==" => Symbol::DoubleEquals,
                        "!=" => Symbol::BangEquals,
                        "=" => Symbol::Equals,
                        "." => Symbol::Dot,
                        "::" => Symbol::DoubleColon,
                        ":" => Symbol::Colon,
                        _ => panic!("Unrecognised symbol {}", symbol)
                    }));
                }
                '0'..='9'  if !is_string => {
                    if word.is_empty() {
                        is_number = true;
                    }
                    word.push(c);
                    col += 1;
                }
                _ if is_number && !is_string => {
                    vec.push(build_identifier(col, row, &word, is_number, is_string));
                    word = "".to_string();
                    is_number = false;
                    is_decimal = false;
                    word.push(c);
                    col += 1;
                }
                _ => {
                    word.push(c);
                    col += 1;
                }
            }
        }
    }

    vec.push(Lexeme::Eof(Span { col, row, length: 0 }));
    Ok(vec)
}