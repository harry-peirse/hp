use std::collections::HashMap;
use std::error::Error;

use crate::parser::{BinaryOp, Expression, Function, UnaryOp};

pub fn interpret(ast: &Vec<Function>) -> Result<(), Box<dyn Error>> {
    if let Some(main) = ast.iter().find(|fun| fun.name == "main") {
        call(main, &vec!(), &ast, &HashMap::new())?;
    } else {
        println!("No main function!")
    }

    Ok(())
}

fn call(fun: &Function, args: &Vec<Expression>, functions: &Vec<Function>, previous_memory: &HashMap<String, String>) -> Result<String, Box<dyn Error>> {
    let mut memory = HashMap::new();
    for (i, arg) in fun.args.iter().enumerate() {
        if let Some(expr) = args.get(i) {
            memory.insert(arg.name.clone(), eval(expr, &functions, previous_memory)?);
        } else {
            panic!("Missing argument {}", arg.name)
        }
    }
    return if let Some(result) = fun.block.iter().map(|expression| eval(&expression, &functions, &memory)).last() {
        result
    } else {
        Ok("void".to_string())
    };
}

fn eval(expression: &Expression, functions: &Vec<Function>, memory: &HashMap<String, String>) -> Result<String, Box<dyn Error>> {
    match expression {
        Expression::LiteralString(string) => Ok(string.to_string()),
        Expression::LiteralNumber(value) => Ok(value.to_string()),
        Expression::Wrapped(nested) => eval(&*nested, &functions, &memory),
        Expression::Unary(op, expression) => match op {
            UnaryOp::Plus => eval(&expression, &functions, &memory),
            UnaryOp::Minus => Ok((-((eval(&expression, &functions, &memory)?).parse::<f64>()?)).to_string()),
            UnaryOp::Not => if ["false", "0", "void", ""].contains(&eval(&expression, &functions, &memory)?.as_str()) { Ok("true".to_string()) } else { Ok("false".to_string()) },
        }
        Expression::If(condition, success, failure) => {
            let result = eval(&condition, &functions, &memory)?;
            if ["void", "false", "0", ""].contains(&result.as_str()) {
                if let Some(result) = failure.iter().map(|expression| eval(&expression, &functions, &memory)).last() {
                    result
                } else {
                    Ok("void".to_string())
                }
            } else {
                if let Some(result) = success.iter().map(|expression| eval(&expression, &functions, &memory)).last() {
                    result
                } else {
                    Ok("void".to_string())
                }
            }
        }
        Expression::Binary(lhs, op, rhs) => match op {
            BinaryOp::Plus => Ok((eval(&*lhs, &functions, &memory)?.parse::<f64>()? + eval(&*rhs, &functions, &memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Minus => Ok((eval(&*lhs, &functions, &memory)?.parse::<f64>()? - eval(&*rhs, &functions, &memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Multiply => Ok((eval(&*lhs, &functions, &memory)?.parse::<f64>()? * eval(&*rhs, &functions, &memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Divide => Ok((eval(&*lhs, &functions, &memory)?.parse::<f64>()? / eval(&*rhs, &functions, &memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Lte => Ok((eval(&*lhs, &functions, &memory)?.parse::<f64>()? <= eval(&*rhs, &functions, &memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Gte => Ok((eval(&*lhs, &functions, &memory)?.parse::<f64>()? >= eval(&*rhs, &functions, &memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Lt => Ok((eval(&*lhs, &functions, &memory)?.parse::<f64>()? < eval(&*rhs, &functions, &memory)?.parse::<f64>()?).to_string()),
            BinaryOp::Gt => Ok((eval(&*lhs, &functions, &memory)?.parse::<f64>()? > eval(&*rhs, &functions, &memory)?.parse::<f64>()?).to_string()),
            _ => panic!("Binary Op not yet implemented {:?}", op)
        }
        Expression::Variable(value) =>
            if let Some(var) = memory.get(value) {
                Ok(var.clone())
            } else {
                panic!("Couldn't find variable in memory {}", value)
            }
        Expression::Call(function_name, args) =>
            if let Some(function) = functions.iter().find(|fun| fun.name.to_string() == function_name.to_string()) {
                call(&function, &args, &functions, memory)
            } else {
                match function_name.as_str() {
                    "print" => if let Some(arg1) = args.first() {
                        println!("{}", eval(&arg1, &functions, &memory)?);
                        Ok("void".to_string())
                    } else {
                        panic!("Incorrect function call for print")
                    }
                    _ => panic!("Couldn't resolve function {}", function_name)
                }
            }
    }
}