use std::collections::HashMap;
use std::error::Error;

use crate::parser::{BinaryOp, Declaration, Expression, Function, Struct, UnaryOp};

pub fn interpret(ast: &Vec<Declaration>) -> Result<(), Box<dyn Error>> {
    if let Some(Declaration::Function(main)) = ast.iter().find(|fun| matches!(fun, Declaration::Function(fun) if fun.name.as_str() == "main")) {
        call(&main, &vec!(), &ast, &mut HashMap::new())?;
    } else {
        println!("No main function!")
    }
    Ok(())
}

fn call(
    fun: &Function,
    parsed_args: &Vec<Expression>,
    space: &Vec<Declaration>,
    previous_memory: &mut HashMap<String, String>
) -> Result<String, Box<dyn Error>> {
    let mut memory = HashMap::new();
    for (i, arg) in fun.args.iter().enumerate() {
        if let Some(expr) = parsed_args.get(i) {
            if let Expression::Named(name, sub_expr) = expr {
                memory.insert(name.clone(), eval(&sub_expr, &space, previous_memory)?);
            } else {
                memory.insert(arg.name.clone(), eval(&expr, &space, previous_memory)?);
            }
        } else {
            panic!("Missing argument {}", arg.name)
        }
    }
    Ok(eval(&fun.body, space, &mut memory)?)
}

fn create(
    str: &Struct,
    parsed_args: &Vec<Expression>,
    space: &Vec<Declaration>,
    previous_memory: &mut HashMap<String, String>,
) -> Result<String, Box<dyn Error>> {
    Ok("done".to_string())
}

fn eval(expression: &Expression, space: &Vec<Declaration>, memory: &mut HashMap<String, String>) -> Result<String, Box<dyn Error>> {
    let mut new_memory = memory.clone();
    match expression {
        Expression::LiteralString(string) => Ok(string.to_string()),
        Expression::LiteralNumber(value) => Ok(value.to_string()),
        Expression::Wrapped(nested) => eval(&*nested, &space, memory),
        Expression::Block(block) => {
            for (_, expr) in block.iter().enumerate().filter(|(i, _)| i.clone() < block.len() - 1) {
                eval(&expr, space, &mut new_memory)?;
            }

            match block.last() {
                Some(expr) => Ok(eval(&expr, &space, &mut new_memory)?),
                _ => Ok("void".to_string())
            }
        }
        Expression::Unary(op, expression) => match op {
            UnaryOp::Plus => eval(expression, space, memory),
            UnaryOp::Minus => Ok((-((eval(expression, space, memory)?).parse::<f64>()?)).to_string()),
            UnaryOp::Not => if ["false", "0", "void", ""].contains(&eval(&expression, &space, memory)?.as_str()) { Ok("true".to_string()) } else { Ok("false".to_string()) },
        }
        Expression::If(condition, success, failure) => {
            let result = eval(condition, space, memory)?;
            if ["void", "false", "0", ""].contains(&result.as_str()) {
                Ok(eval(&failure, space, memory)?)
            } else {
                Ok(eval(&success, space, memory)?)
            }
        }
        Expression::Declare(name, expr) => {
            let result = eval(expr, space, memory)?;
            memory.insert(name.clone(), result.clone());
            Ok(result)
        }
        Expression::Binary(lhs, op, rhs) => match op {
            BinaryOp::Plus => Ok((eval(&*lhs, &space, memory)?.parse::<f64>()? + eval(&*rhs, space, memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Minus => Ok((eval(&*lhs, &space, memory)?.parse::<f64>()? - eval(&*rhs, &space, memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Multiply => Ok((eval(&*lhs, &space, memory)?.parse::<f64>()? * eval(&*rhs, &space, memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Divide => Ok((eval(&*lhs, &space, memory)?.parse::<f64>()? / eval(&*rhs, &space, memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Lte => Ok((eval(&*lhs, &space, memory)?.parse::<f64>()? <= eval(&*rhs, &space, memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Gte => Ok((eval(&*lhs, &space, memory)?.parse::<f64>()? >= eval(&*rhs, &space, memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Lt => Ok((eval(&*lhs, &space, memory)?.parse::<f64>()? < eval(&*rhs, &space, memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Gt => Ok((eval(&*lhs, &space, memory)?.parse::<f64>()? > eval(&*rhs, &space, memory)?.parse::<f64>()?).to_string()),
            BinaryOp::NotEquals => Ok((eval(&*lhs, &space, memory)? != eval(&*rhs, &space, memory)?).to_string()),
            BinaryOp::Equals => Ok((eval(&*lhs, &space, memory)? == eval(&*rhs, &space, memory)?).to_string()),
            _ => panic!("Binary Op not yet implemented {:?}", op)
        }
        Expression::Variable(value) =>
            if let Some(var) = memory.get(value) {
                Ok(var.clone())
            } else {
                panic!("Couldn't find variable in memory {}", value)
            }
        Expression::Call(function_name, args) =>
            match space.iter()
                .find(|dec|
                    match dec {
                        Declaration::Function(fun) if fun.name == function_name.to_string() => true,
                        Declaration::Struct(str) if str.name == function_name.to_string() => true,
                        _ => false
                    }) {
                Some(Declaration::Function(function)) =>
                    call(&function, &args, &space, memory),
                Some(Declaration::Struct(str)) =>
                    create(&str, &args, &space, memory),
                _ =>
                    match function_name.as_str() {
                        "print" => if let Some(arg1) = args.first() {
                            println!("{}", eval(&arg1, &space, memory)?);
                            Ok("void".to_string())
                        } else {
                            panic!("Incorrect function call for print")
                        }
                        _ => panic!("Couldn't resolve function {}", function_name)
                    }
            }
        Expression::Named(_, _) => panic!("Tried to pass Expression::Named!")
    }
}