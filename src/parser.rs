use std::error::Error;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::{Keyword, Lexeme, Symbol};

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub args: Box<Vec<Argument>>,
    pub body: Expression,
    pub return_type: Option<String>
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Box<Vec<Argument>>,
}

impl Display for Struct {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: String,
    pub type_name: String,
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(Function),
    Struct(Struct),
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self)
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Call(Option<String>, String, Box<Vec<Expression>>),
    Wrapped(Option<String>, Box<Expression>),
    LiteralString(String),
    LiteralNumber(String),
    If(Option<String>, Box<Expression>, Box<Expression>, Box<Expression>),
    Variable(Option<String>, String),
    Binary(Option<String>, Box<Expression>, BinaryOp, Box<Expression>),
    Unary(Option<String>, UnaryOp, Box<Expression>),
    Declare(String, Box<Expression>),
    Block(Option<String>, Box<Vec<Expression>>),
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

pub fn parse(lexemes: Vec<Lexeme>) -> Result<Vec<Declaration>, Box<dyn Error>> {
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
                        vec.push(parse_declaration(&mut iter)?)
                    }
                }
                next = iter.peek();
            }
            None => ()
        }
    }

    Ok(vec)
}

fn parse_declaration(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Declaration, Box<dyn Error>> {
    let name = expect_identifier(iter)?;
    skip_newline(iter);
    expect_symbol(iter, Symbol::DoubleColon);
    skip_newline(iter);

    let mut args = parse_arguments(iter)?;

    skip_newline(iter);
    if let Some(Lexeme::Symbol(_, Symbol::EqualsRightArrow)) = iter.peek() {
        expect_symbol(iter, Symbol::EqualsRightArrow);
        Ok(Declaration::Function(Function { name, args: Box::new(args), body: parse_expression(iter)? , return_type: None}))
    } else {
        Ok(Declaration::Struct(Struct { name, fields: Box::new(args) }))
    }
}

fn parse_arguments(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Vec<Argument>, Box<dyn Error>> {
    let mut args = vec!();
    if let Some(Lexeme::Symbol(_, Symbol::OpenBracket)) = iter.peek() {
        expect_symbol(iter, Symbol::OpenBracket);
        skip_newline(iter);
        while !is_symbol(iter, Symbol::CloseBracket) {
            args.push(parse_argument(iter)?);
            match iter.peek() {
                Some(Lexeme::Newline(_)) | Some(Lexeme::Symbol(_, Symbol::CloseBracket)) => (),
                _ => expect_symbol(iter, Symbol::Comma)
            }
            skip_newline(iter);
        }
        expect_symbol(iter, Symbol::CloseBracket);
    }
    Ok(args)
}

fn parse_argument(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Argument, Box<dyn Error>> {
    let name = expect_identifier(iter)?;
    expect_symbol(iter, Symbol::Colon);
    let type_name = expect_identifier(iter)?;
    Ok(Argument { name, type_name })
}

// TODO(harry): This precedence work has a big bug. 1 * 2 + 3 < 4 is parsed incorrectly.
fn parse_expression(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Expression, Box<dyn Error>> {
    let mut expression: Expression = parse_non_binary_expression(iter)?;
    loop {
        let binary_op = match iter.peek() {
            Some(Lexeme::Symbol(_, sym)) => match sym {
                Symbol::Plus => Some(BinaryOp::Plus),
                Symbol::Dash => Some(BinaryOp::Minus),
                Symbol::Asterisk => Some(BinaryOp::Multiply),
                Symbol::ForwardSlash => Some(BinaryOp::Divide),
                Symbol::LeftArrowEquals => Some(BinaryOp::Lte),
                Symbol::RightArrowEquals => Some(BinaryOp::Gte),
                Symbol::LeftArrow => Some(BinaryOp::Lt),
                Symbol::RightArrow => Some(BinaryOp::Gt),
                Symbol::DoubleEquals => Some(BinaryOp::Equals),
                Symbol::BangEquals => Some(BinaryOp::NotEquals),
                Symbol::Equals => Some(BinaryOp::Assign),
                Symbol::DoubleAmpersand => Some(BinaryOp::And),
                Symbol::DoubleBar => Some(BinaryOp::Or),
                Symbol::Dot => Some(BinaryOp::Dot),
                _ => None
            }
            _ => None
        };
        if let Some(next_op) = binary_op {
            iter.next();
            let next = parse_non_binary_expression(iter)?;
            let mut previous = &mut expression;
            loop {
                if let Expression::Binary(None, ref mut lhs, ref mut previous_op, ref mut rhs) = previous {
                    if binary_op_precedence(&next_op) > binary_op_precedence(&previous_op) {
                        previous = rhs;
                    } else {
                        *lhs = Box::new(Expression::Binary(None, Box::new(*lhs.clone()), previous_op.clone(), Box::new(*rhs.clone())));
                        *previous_op = next_op.clone();
                        *rhs = Box::new(next);
                        break;
                    }
                } else {
                    *previous = Expression::Binary(None, Box::new(previous.clone()), next_op, Box::new(next));
                    break;
                }
            }
        } else {
            break;
        }
    }
    Ok(expression)
}

fn parse_string_template(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Expression, Box<dyn Error>> {
    let mut expr: Option<Expression> = None;
    while let Some(_) = iter.peek() {
        expr = Some(match expr {
            None => Expression::Wrapped(None, Box::new(parse_expression(iter)?)),
            Some(previous_expr) =>
                Expression::Wrapped(None, Box::new(Expression::Binary(None,
                                                                      Box::new(previous_expr),
                    BinaryOp::Plus,
                    Box::new(Expression::Wrapped(None, Box::new(parse_expression(iter)?))))))
        })
    }
    if let Some(expr) = expr {
        Ok(expr)
    } else {
        panic!("Empty String Template")
    }
}

fn parse_non_binary_expression(iter: &mut Peekable<Iter<Lexeme>>) -> Result<Expression, Box<dyn Error>> {
    if let Some(lexeme) = iter.next() {
        Ok(match lexeme {
            Lexeme::String(_, value) => Expression::LiteralString(value.clone()),
            Lexeme::StringTemplate(_, tokens) => parse_string_template(&mut tokens.iter().peekable())?,
            Lexeme::Number(_, value) => Expression::LiteralNumber(value.clone()),
            Lexeme::Identifier(_, identifier) => {
                match iter.peek() {
                    Some(Lexeme::Symbol(_, sym)) => {
                        match sym {
                            Symbol::OpenBracket => {
                                // Function call
                                iter.next();
                                let mut args = vec!();
                                while !is_symbol(iter, Symbol::CloseBracket) {
                                    args.push(parse_expression(iter)?);
                                    if !is_symbol(iter, Symbol::CloseBracket) {
                                        expect_symbol(iter, Symbol::Comma)
                                    }
                                }
                                expect_symbol(iter, Symbol::CloseBracket);
                                Expression::Call(None, identifier.clone(), Box::new(args))
                            }
                            _ => Expression::Variable(None, identifier.clone())
                        }
                    }
                    _ => Expression::Variable(None, identifier.clone())
                }
            }
            Lexeme::Keyword(_, Keyword::If) => {
                let condition = parse_expression(iter)?;
                expect_keyword(iter, Keyword::Then);
                let success_case = parse_expression(iter)?;
                expect_keyword(iter, Keyword::Else);
                let failure_case = parse_expression(iter)?;
                Expression::If(None, Box::new(condition), Box::new(success_case), Box::new(failure_case))
            }
            Lexeme::Keyword(_, Keyword::Let) => {
                let name = expect_identifier(iter)?;
                expect_symbol(iter, Symbol::Equals);
                Expression::Declare( name, Box::new(parse_expression(iter)?))
            }
            Lexeme::Symbol(_, Symbol::Plus) => Expression::Unary(None, UnaryOp::Plus, Box::new(parse_non_binary_expression(iter)?)),
            Lexeme::Symbol(_, Symbol::Dash) => Expression::Unary(None, UnaryOp::Minus, Box::new(parse_non_binary_expression(iter)?)),
            Lexeme::Symbol(_, Symbol::Bang) => Expression::Unary(None, UnaryOp::Not, Box::new(parse_non_binary_expression(iter)?)),
            Lexeme::Symbol(_, Symbol::OpenBracket) => {
                let result = parse_expression(iter)?;
                expect_symbol(iter, Symbol::CloseBracket);
                Expression::Wrapped(None, Box::new(result))
            }
            Lexeme::Symbol(_, Symbol::OpenBrace) => {
                let mut block = vec!();
                skip_newline(iter);
                while !is_symbol(iter, Symbol::CloseBrace) {
                    block.push(parse_expression(iter)?);
                    skip_newline(iter);
                }
                expect_symbol(iter, Symbol::CloseBrace);
                skip_newline(iter);
                Expression::Block(None, Box::new(block))
            }
            it => panic!("Unsupported Operation {}", it)
        })
    } else {
        panic!("Token stream empty")
    }
}

fn skip_newline(iter: &mut Peekable<Iter<Lexeme>>) {
    if let Some(Lexeme::Newline(_)) = iter.peek() {
        iter.next();
    }
}

fn expect_identifier(iter: &mut Peekable<Iter<Lexeme>>) -> Result<String, Box<dyn Error>> {
    match iter.next() {
        Some(Lexeme::Identifier(_, it)) => Ok(it.clone()),
        Some(it) => panic!("Expected Identifier but got {}", it),
        None => panic!("Expected Identifier but ran out of Lexemes")
    }
}

fn expect_symbol(iter: &mut Peekable<Iter<Lexeme>>, sym: Symbol) {
    match iter.next() {
        Some(Lexeme::Symbol(_, it)) if sym == *it => (),
        it => panic!("Expected {} but it was {:?}", sym, it)
    };
}

fn expect_keyword(iter: &mut Peekable<Iter<Lexeme>>, keyword: Keyword) {
    match iter.next() {
        Some(Lexeme::Keyword(_, it)) if *it == keyword => (),
        _ => panic!("Expected {} but it was missing", keyword)
    };
}

fn is_symbol(iter: &mut Peekable<Iter<Lexeme>>, symbol: Symbol) -> bool {
    matches!(iter.peek(), Some(Lexeme::Symbol(_, it)) if *it == symbol)
}