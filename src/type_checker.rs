use std::collections::HashMap;
use std::error::Error;

use crate::parser::{BinaryOp, Declaration, Expression, Function, Struct};

pub fn type_check(declarations: &mut Vec<Declaration>) -> Result<(), Box<dyn Error>> {
    for declaration in declarations {
        match declaration {
            Declaration::Function(fun) => type_check_function(fun)?,
            _ => ()
        }
    }
    Ok(())
}

fn type_check_function(fun: &mut Function) -> Result<(), Box<dyn Error>> {
    let mut type_table = HashMap::new();
    fun.return_type = Some(type_check_expression(&mut fun.body, &mut type_table)?);
    Ok(())
}

fn type_check_expression(expr: &mut Expression, type_table: &mut HashMap<String, String>) -> Result<String, Box<dyn Error>> {
    match expr {
        Expression::Binary(_type, lhs, op, rhs) => {
            let lhs_type = type_check_expression(lhs, type_table)?;
            let rhs_type = type_check_expression(rhs, type_table)?;
            match op {
                BinaryOp::Equals | BinaryOp::NotEquals | BinaryOp::And | BinaryOp::Or | BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Lte | BinaryOp::Gte => {
                    *_type = Some("bool".to_string());
                    Ok("bool".to_string())
                }
                BinaryOp::Minus | BinaryOp::Multiply | BinaryOp::Divide => {
                    *_type = Some("number".to_string());
                    Ok("number".to_string())
                }
                BinaryOp::Plus => {
                    if rhs_type == "string" || lhs_type == "string" {
                        *_type = Some("string".to_string());
                        Ok("string".to_string())
                    } else {
                        *_type = Some("number".to_string());
                        Ok("number".to_string())
                    }
                }
                BinaryOp::Assign | BinaryOp::PlusAssign | BinaryOp::MinusAssign | BinaryOp::MultiplyAssign | BinaryOp::DivideAssign => {
                    *_type = Some("void".to_string());
                    Ok("void".to_string())
                }
                BinaryOp::Dot => {
                    *_type = Some(rhs_type.clone());
                    Ok(rhs_type)
                }
            }
        }
        Expression::LiteralString(_) => Ok("string".to_string()),
        Expression::LiteralNumber(_) => Ok("number".to_string()),
        Expression::Variable(_type, name) => {
            let result = type_table.get(name).expect(format!("Can't resolve type of variable {}", name).as_str());
            *_type = Some(result.clone());
            Ok(result.clone())
        }
        Expression::Loop(counter, child) => {
            type_check_expression(counter, type_table)?;
            type_check_expression(child, type_table)?;
            Ok("void".to_string())
        },
        Expression::Wrapped(_type, child) => {
            let result = type_check_expression(child, type_table)?;
            *_type = Some(result.clone());
            Ok(result.clone())
        }
        Expression::Call(_type, name, args) => {
            let result = match name.as_str() {
                "write" => "void",
                "read" => "string",
                _ => {
                    "unknown"
                }
            }.to_string();
            for arg in args.iter_mut() {
                type_check_expression(arg, type_table)?;
            }
            *_type = Some(result.clone());
            Ok(result.clone())
        }
        Expression::If(_type, cond, success, failure) => {
            let cond_type = type_check_expression(cond, type_table)?;
            if cond_type != "bool" {
                panic!("If condition is not of a bool type")
            }
            let success_type = type_check_expression(success, &mut type_table.clone())?;
            let failure_type = type_check_expression(failure, &mut type_table.clone())?;
            if success_type != failure_type {
                panic!("If branches don't match types")
            } else {
                Ok("unknown".to_string())
            }
        }
        Expression::Unary(_type, op, child) => {
            let child_type = type_check_expression(child, type_table)?;
            Ok(child_type)
        }
        Expression::Declare(name, child) => {
            let result = type_check_expression(child, type_table)?;
            type_table.insert(name.clone(), result);
            Ok("void".to_string())
        }
        Expression::Block(_type, child) => {
            let mut new_type_table = type_table.clone();
            let length = child.len();
            for (i, it) in child.iter_mut().enumerate() {
                if i < length - 1 {
                    type_check_expression(it, &mut new_type_table)?;
                }
            }
            if let Some(last) = child.last_mut() {
                let child_type = type_check_expression(last, &mut new_type_table)?;
                *_type = Some(child_type.clone());
                Ok(child_type)
            } else {
                *_type = Some("void".to_string());
                Ok("void".to_string())
            }
        }
    }
}