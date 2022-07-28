use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::Lexeme;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub args: Vec<Argument>,
    pub return_type_name: Option<String>,
    pub block: Vec<Expression>,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug)]
pub struct Argument {
    pub name: String,
    pub type_name: String,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Call(String, Box<Vec<Expression>>),
    Wrapped(Box<Expression>),
    LiteralString(String),
    LiteralNumber(String),
    If(Box<Expression>, Box<Vec<Expression>>, Box<Vec<Expression>>),
    Variable(String),
    Binary(Box<Expression>, BinaryOp, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    Or,
    Lt,
    Gt,
    Lte,
    Gte,
    Equals,
    NotEquals,
    Assign,
    Dot,
}

// These values are arbitrary, and only matter in directional comparison to each other
fn binary_op_precedence(op: &BinaryOp) -> u32 {
    match op {
        BinaryOp::Assign => 10,
        BinaryOp::Or => 20,
        BinaryOp::And => 30,
        BinaryOp::Equals => 40,
        BinaryOp::NotEquals => 40,
        BinaryOp::Lt => 50,
        BinaryOp::Gt => 50,
        BinaryOp::Lte => 50,
        BinaryOp::Gte => 50,
        BinaryOp::Plus => 60,
        BinaryOp::Minus => 60,
        BinaryOp::Multiply => 70,
        BinaryOp::Divide => 70,
        BinaryOp::Dot => 80
    }
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

// TODO(harry): This precedence work has a big bug. 1 * 2 + 3 < 4 is parsed incorrectly.
fn parse_expression(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Expression, Box<dyn Error>> {
    let mut expression: Expression = parse_non_binary_expression(iter)?;
    loop {
        let binary_op = match iter.peek() {
            Some(Lexeme::Symbol(_, sym)) => match sym.as_str() {
                "+" => Some(BinaryOp::Plus),
                "-" => Some(BinaryOp::Minus),
                "*" => Some(BinaryOp::Multiply),
                "/" => Some(BinaryOp::Divide),
                "<=" => Some(BinaryOp::Lte),
                ">=" => Some(BinaryOp::Gte),
                "<" => Some(BinaryOp::Lt),
                ">" => Some(BinaryOp::Gt),
                "==" => Some(BinaryOp::Equals),
                "!=" => Some(BinaryOp::NotEquals),
                "=" => Some(BinaryOp::Assign),
                "&&" => Some(BinaryOp::And),
                "||" => Some(BinaryOp::Or),
                "." => Some(BinaryOp::Dot),
                _ => None
            }
            _ => None
        };
        if let Some(next_op) = binary_op {
            iter.next();
            let next = parse_non_binary_expression(iter)?;
            let mut previous = &mut expression;
            loop {
                if let Expression::Binary(ref mut lhs, ref mut previous_op, ref mut rhs) = previous {
                    if binary_op_precedence(&next_op) > binary_op_precedence(&previous_op) {
                        previous = rhs;
                    } else {
                        *lhs = Box::new(Expression::Binary(Box::new(*lhs.clone()), previous_op.clone(), Box::new(*rhs.clone())));
                        *previous_op = next_op.clone();
                        *rhs = Box::new(next);
                        break;
                    }
                } else {
                    *previous = Expression::Binary(Box::new(previous.clone()), next_op, Box::new(next));
                    break;
                }
            }
        } else {
            break;
        }
    }
    Ok(expression)
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
        Some(Lexeme::Symbol(_, sym)) => match sym.as_str() {
            "+" => Ok(Expression::Unary(UnaryOp::Plus, Box::new(parse_non_binary_expression(iter)?))),
            "-" => Ok(Expression::Unary(UnaryOp::Minus, Box::new(parse_non_binary_expression(iter)?))),
            "!" => Ok(Expression::Unary(UnaryOp::Not, Box::new(parse_non_binary_expression(iter)?))),
            "(" => {
                let result = parse_expression(iter)?;
                expect_symbol(iter, ")");
                Ok(Expression::Wrapped(Box::new(result)))
            }
            _ => panic!("Unexpected Symbol {}", sym)
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