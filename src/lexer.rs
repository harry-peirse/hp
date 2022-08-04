use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::Chars;

#[derive(Clone)]
#[derive(Debug)]
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

#[derive(Debug)]
pub enum Lexeme {
    Identifier(Span, String),
    Symbol(Span, Symbol),
    Keyword(Span, Keyword),
    Number(Span, String),
    String(Span, String),
    StringTemplate(Span, Box<Vec<Lexeme>>),
    Newline(Span),
    Eof(Span),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Keyword {
    If,
    Else,
    Loop,
    Let,
    True,
    False,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", match self {
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Loop => "loop",
            Keyword::Let => "let",
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
    PlusEquals,
    DashEquals,
    AsteriskEquals,
    ForwardSlashEquals,
    Dot,
    Colon,
    DoubleColon,
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
            Symbol::PlusEquals => "+=",
            Symbol::DashEquals => "-=",
            Symbol::AsteriskEquals => "*=",
            Symbol::ForwardSlashEquals => "/=",
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
            Lexeme::StringTemplate(pos, parts) => write!(f, "{} TEMPLATE {}", pos, parts.iter().map(|it| format!("\n               {}", it)).collect::<String>()),
            Lexeme::Keyword(pos, keyword) => write!(f, "{} KEY  {}", pos, keyword)
        }
    }
}

fn build_identifier(col: u64, row: u64, word: &String, is_number: bool) -> Result<Lexeme, Box<dyn Error>> {
    let word_length = word.len() as u64;
    let pos = Span { col: col - word_length as u64, row: row.clone(), length: word_length };
    Ok(if is_number {
        Lexeme::Number(pos, word.clone())
    } else {
        match word.as_str() {
            "if" => Lexeme::Keyword(pos, Keyword::If),
            "else" => Lexeme::Keyword(pos, Keyword::Else),
            "loop" => Lexeme::Keyword(pos, Keyword::Loop),
            "is" => Lexeme::Symbol(pos, Symbol::DoubleEquals),
            "isnt" => Lexeme::Symbol(pos, Symbol::BangEquals),
            "or" => Lexeme::Symbol(pos, Symbol::DoubleBar),
            "and" => Lexeme::Symbol(pos, Symbol::DoubleAmpersand),
            "let" => Lexeme::Keyword(pos, Keyword::Let),
            "true" => Lexeme::Keyword(pos, Keyword::True),
            "false" => Lexeme::Keyword(pos, Keyword::False),
            _ => Lexeme::Identifier(pos, word.clone())
        }
    })
}


pub fn lex(code: String) -> Result<Vec<Lexeme>, Box<dyn Error>> {
    lex_module(1, 1, &mut code.chars().peekable())
}

fn lex_module(mut col: u64, mut row: u64, iter: &mut Peekable<Chars>) -> Result<Vec<Lexeme>, Box<dyn Error>> {
    let mut vec = vec!();
    let mut word: String = "".to_string();
    let mut is_number = false;
    let mut is_decimal = false;

    while let Some(c) = iter.next() {
        match c {
            '\r' => (),
            '\n' => {
                if !word.is_empty() {
                    vec.push(build_identifier(col, row, &word, is_number)?);
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
                col = 1;
                row += 1;
            }
            ' ' => {
                if !word.is_empty() {
                    vec.push(build_identifier(col, row, &word, is_number)?);
                    word = "".to_string();
                }
                is_number = false;
                is_decimal = false;
                col += 1;
            }
            '.' if is_number && !is_decimal => {
                word.push(c);
                is_decimal = true;
                col += 1;
            }
            '"' => {
                if !word.is_empty() {
                    vec.push(build_identifier(col, row, &word, is_number)?);
                    word = "".to_string();
                }
                vec.push(Lexeme::StringTemplate(Span { col, row, length: word.len() as u64 }, Box::new(lex_string_template(col, row, iter)?)));
                is_number = false;
                is_decimal = false;
            }
            '{' | '}' | '(' | ')' | '[' | ']' | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | ':' | '|' | '&' | ',' | '.' | ';' => {
                if !word.is_empty() {
                    vec.push(build_identifier(col, row, &word, is_number)?);
                    word = "".to_string();
                }
                is_number = false;
                is_decimal = false;

                let mut symbol = c.to_string();
                if ['+', '-', '*', '/', '=', '<', '>', '!', ':', '|', '&'].contains(&c) {
                    match iter.peek() {
                        Some(next_c) => {
                            if let '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | ':' | '|' | '&' = next_c {
                                symbol.push(next_c.clone());
                                iter.next();
                                col += 1;
                            }
                        }
                        None => ()
                    }
                }

                if symbol == "//" {
                    word = String::new();
                    while let Some(c) = iter.peek() {
                        if c != &'\n' {
                            iter.next();
                        } else {
                            break;
                        }
                    }
                } else {
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
                        "+=" => Symbol::PlusEquals,
                        "-=" => Symbol::DashEquals,
                        "*=" => Symbol::AsteriskEquals,
                        "/=" => Symbol::ForwardSlashEquals,
                        "." => Symbol::Dot,
                        "::" => Symbol::DoubleColon,
                        ":" => Symbol::Colon,
                        _ => panic!("Unrecognised symbol {}", symbol)
                    }));
                }
            }
            '0'..='9' => {
                if word.is_empty() {
                    is_number = true;
                }
                word.push(c);
                col += 1;
            }
            _ if is_number => {
                vec.push(build_identifier(col, row, &word, is_number)?);
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

    println!("{:?} {:?}", iter, word);

    vec.push(Lexeme::Eof(Span { col, row, length: 0 }));
    Ok(vec)
}

fn lex_string_template(mut col: u64, mut row: u64, iter: &mut Peekable<Chars>) -> Result<Vec<Lexeme>, Box<dyn Error>> {
    let mut vec = vec!();
    let mut word = String::new();
    let mut is_var = false;
    while let Some(c) = iter.next() {
        col += 1;
        match c {
            '\r' => (),
            '"' => {
                if is_var {
                    vec.push(Lexeme::Identifier(Span { col, row, length: word.len() as u64 }, word.clone()));
                    word = String::new();
                } else {
                    vec.push(Lexeme::String(Span { col, row, length: word.len() as u64 }, word.clone()));
                    word = String::new();
                }
                break;
            }
            '$' if !is_var => {
                match iter.peek() {
                    Some('$') => {
                        word.push('$');
                        iter.next();
                    }
                    _ => {
                        is_var = true;
                        vec.push(Lexeme::String(Span { col, row, length: word.len() as u64 }, word.clone()));
                        word = "".to_string();
                    }
                }
            }
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' if is_var => {
                word.push(c)
            }
            _ if is_var => {
                vec.push(Lexeme::Identifier(Span { col, row, length: word.len() as u64 }, word.clone()));
                word = String::new();
                is_var = false;
            }
            _ => {
                word.push(c)
            }
        }
    }
    Ok(vec)
}