use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::Lexeme;

#[derive(Debug)]
pub struct Function {
    name: String,
    args: Vec<Argument>,
    return_type_name: Option<String>,
    block: Vec<Expression>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug)]
pub struct Argument {
    name: String,
    type_name: String,
}

#[derive(Debug)]
pub enum Expression {
    Call(String, Box<Vec<Expression>>),
    LiteralString(String),
    LiteralNumber(String),
    Return(Option<Box<Expression>>),
    If(Box<Expression>, Box<Vec<Expression>>, Box<Vec<Expression>>),
    Variable(String),
    BinaryOp(Box<Expression>, BinaryOp, Box<Expression>),
}

#[derive(Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    // Multiply,
    // Divide,
    // And,
    // Or,
    // Lt,
    // Gt,
    Lte,
    // Gte,
    // Equals,
    // NotEquals,
    // Assign,
    // Dot
}

pub fn parse(lexemes: Vec<Lexeme>) -> Result<Vec<Function>, Box<dyn Error>> {
    let mut vec = vec!();

    let mut iter = lexemes.iter().peekable();
    let mut next = iter.peek();
    while next.is_some() {
        match next {
            Some(token) => {
                match token {
                    Lexeme::Newline(_) | Lexeme::Eof(_) => {
                        iter.next();
                        next = iter.peek();
                    }
                    _ => {
                        vec.push(parse_function(&mut iter)?)
                    }
                }
                next = iter.peek();
            }
            None => ()
        }
    }

    Ok(vec)
}

fn parse_function(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Function, Box<dyn Error>> {
    let name = expect_identifier(iter)?;

    expect_symbol(iter, "::");
    skip_newline(iter);

    let mut args = vec!();
    let has_args = is_symbol(iter, "(");
    if has_args {
        expect_symbol(iter, "(");
        while !is_symbol(iter, ")") {
            args.push(parse_argument(iter)?);
            if !is_symbol(iter, ")") {
                expect_symbol(iter, ",")
            }
        }
        expect_symbol(iter, ")");
    }

    let is_block = is_symbol(iter, "{");

    let mut block = vec!();
    if is_block {
        expect_symbol(iter, "{");
        skip_newline(iter);

        while !is_symbol(iter, "}") {
            block.push(parse_expression(iter)?);
            skip_newline(iter);
        }
        expect_symbol(iter, "}");
        skip_newline(iter);
    } else {
        expect_symbol(iter, "=>");
        skip_newline(iter);

        block.push(parse_expression(iter)?);
        skip_newline(iter);
    }

    Ok(Function { name, args, return_type_name: None, block })
}

fn parse_argument(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Argument, Box<dyn Error>> {
    let name = expect_identifier(iter)?;
    expect_symbol(iter, ":");
    let type_name = expect_identifier(iter)?;
    Ok(Argument { name, type_name })
}

// TODO(harry): Handle operator precedence (note that current behaviour evaluated in the _reverse_ order of how it is written in the input source code)
fn parse_expression(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Expression, Box<dyn Error>> {
    let lhs = parse_non_binary_expression(iter)?;
    let binary_op = match iter.peek() {
        Some(Lexeme::Symbol(_, sym)) => match sym.as_str() {
            "+" => Some(BinaryOp::Plus),
            "-" => Some(BinaryOp::Minus),
            "<=" => Some(BinaryOp::Lte),
            _ => None
        }
        _ => None
    };
    Ok(match binary_op {
        Some(op) => {
            iter.next();
            let rhs = parse_expression(iter)?;
            Expression::BinaryOp(Box::new(lhs), op, Box::new(rhs))
        }
        None => lhs
    })
}

fn parse_non_binary_expression(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Expression, Box<dyn Error>> {
    match iter.next() {
        Some(Lexeme::String(_, value)) => Ok(Expression::LiteralString(value.clone())),
        Some(Lexeme::Number(_, value)) => Ok(Expression::LiteralNumber(value.clone())),
        Some(Lexeme::Identifier(_, identifier)) => {
            match iter.peek() {
                Some(Lexeme::Symbol(_, sym)) => {
                    match &**sym {
                        "(" => {
                            // Function call
                            iter.next();
                            let mut args = vec!();
                            while !is_symbol(iter, ")") {
                                args.push(parse_expression(iter)?);
                                if !is_symbol(iter, ")") {
                                    expect_symbol(iter, ",")
                                }
                            }
                            expect_symbol(iter, ")");
                            Ok(Expression::Call(identifier.clone(), Box::new(args)))
                        }
                        _ => Ok(Expression::Variable(identifier.clone()))
                    }
                }
                _ => Ok(Expression::Variable(identifier.clone()))
            }
        }
        Some(Lexeme::Keyword(_, keyword)) if keyword == "if" => {
            let condition = parse_expression(iter)?;
            expect_keyword(iter, "then");
            let success_case = parse_expression(iter)?;
            expect_keyword(iter, "else");
            let failure_case = parse_expression(iter)?;
            Ok(Expression::If(Box::new(condition), Box::new(vec!(success_case)), Box::new(vec!(failure_case))))
        }
        Some(it) => panic!("Unsupported Operation {}", it),
        None => panic!("Token stream empty")
    }
}

fn skip_newline(iter: &mut Peekable<Iter<Lexeme>>) {
    match iter.peek() {
        Some(Lexeme::Newline(_)) => { iter.next(); }
        _ => ()
    };
}

fn expect_identifier(iter: &mut Peekable<Iter<Lexeme>>) -> Result<String, Box<dyn Error>> {
    match iter.next() {
        Some(Lexeme::Identifier(_, it)) => Ok(it.clone()),
        Some(it) => panic!("Expected Identifier but got {}", it),
        None => panic!("Expected Identifier but ran out of Lexemes")
    }
}

fn expect_symbol(iter: &mut Peekable<Iter<Lexeme>>, token: &str) {
    match iter.next() {
        Some(Lexeme::Symbol(_, it)) if it == token => (),
        _ => panic!("Expected {} but it was missing", token)
    };
}

fn expect_keyword(iter: &mut Peekable<Iter<Lexeme>>, token: &str) {
    match iter.next() {
        Some(Lexeme::Keyword(_, it)) if it == token => (),
        _ => panic!("Expected {} but it was missing", token)
    };
}

fn is_identifier(iter: &mut Peekable<Iter<Lexeme>>) -> bool {
    match iter.peek() {
        Some(Lexeme::Identifier(_, _)) => true,
        _ => false
    }
}

fn is_symbol(iter: &mut Peekable<Iter<Lexeme>>, token: &str) -> bool {
    match iter.peek() {
        Some(Lexeme::Symbol(_, it)) if it == token => true,
        _ => false
    }
}