use std::fmt::{Display, Formatter};

pub struct CodePos {
    col: u64,
    row: u64,
}

impl Display for CodePos {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:>4}:{:<4}", self.row, self.col)
    }
}

pub enum Lexeme {
    IDENTIFIER(CodePos, String),
    SYMBOL(CodePos, String),
    KEYWORD(CodePos, String),
    NUMBER(CodePos, String),
    STRING(CodePos, String),
    NEWLINE(CodePos),
    EOF(CodePos),
}

impl Display for Lexeme {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Lexeme::EOF(pos) => write!(f, "{} EOF", pos),
            Lexeme::NEWLINE(pos) => write!(f, "{} LINE", pos),
            Lexeme::IDENTIFIER(pos, id) => write!(f, "{} ID   {}", pos, id),
            Lexeme::SYMBOL(pos, id) => write!(f, "{} SYM  {}", pos, id),
            Lexeme::NUMBER(pos, id) => write!(f, "{} NUM  {}", pos, id),
            Lexeme::STRING(pos, id) => write!(f, "{} STR  {}", pos, id),
            Lexeme::KEYWORD(pos, id) => write!(f, "{} KEY  {}", pos, id)
        }
    }
}

fn build_identifier(col: u64, row: u64, word: &String, is_number: bool, is_string: bool) -> Lexeme {
    let word_length = word.len() as u64;
    let pos = CodePos { col: col - word_length as u64, row };
    if is_number {
        Lexeme::NUMBER(pos, word.clone())
    } else if is_string {
        Lexeme::STRING(pos, word.clone())
    } else if ["and", "or", "is", "it", "true", "false", "loop", "if"].contains(&&**word) {
        Lexeme::KEYWORD(pos, word.clone())
    } else {
        Lexeme::IDENTIFIER(pos, word.clone())
    }
}

pub fn lex(code: String) -> Vec<Lexeme> {
    let mut vec = Vec::new();
    let mut col: u64 = 0;
    let mut row: u64 = 0;
    let mut word: String = "".to_string();
    let mut is_number = false;
    let mut is_decimal = false;
    let mut skip = false;
    let mut is_string = false;

    code.chars().enumerate().for_each(|(i, c)|
        if skip {
            skip = false;
        } else {
            match c {
                '\n' if !is_string => {
                    if !word.is_empty() {
                        vec.push(build_identifier(col, row, &word, is_number, is_string));
                    }
                    match vec.last() {
                        None => (),
                        Some(last) => {
                            if !matches!(last, Lexeme::NEWLINE(_)) {
                                vec.push(Lexeme::NEWLINE(CodePos { col, row }));
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
                '{' | '}' | '(' | ')' | '[' | ']' | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | ':' | '|' | '&' | ',' | '.'  if !is_string => {
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

                    vec.push(Lexeme::SYMBOL(CodePos { col, row }, symbol));
                }
                '0'..='9'  if !is_string => {
                    if word.is_empty() {
                        is_number = true;
                    }
                    word.push(c);
                    col += 1;
                }
                _ if is_number && !is_string=> {
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
    );

    vec.push(Lexeme::EOF(CodePos { col, row }));
    vec
}