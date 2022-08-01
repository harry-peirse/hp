use crate::parser::{BinaryOp, Expression, Declaration, UnaryOp, Function, Struct};

pub fn generate_node(ast: &Vec<Declaration>) -> String {
    let functions = ast.iter().map(|it| match it {
        Declaration::Function(fun) => generate_function(fun),
        Declaration::Struct(str) => generate_struct(str)
    }).collect::<Vec<String>>().join("\n\n");
    format!("{}\n\nmain();

function print(arg) {{
  console.log(arg);
  return arg;
}}", functions)
}

fn generate_function(fun: &Function) -> String {
    let args = fun.args.iter().map(|it| it.name.clone()).collect::<Vec<String>>().join(", ");
    format!("function {}({}) {{
  return {};
}}", fun.name, args, generate_expression(&fun.body, 1))
}

fn generate_struct(str: &Struct) -> String {
    let args = str.fields.iter().map(|it| it.name.clone()).collect::<Vec<String>>().join(", ");
    format!("function {}({}) {{
  return {{{}}};
}}", str.name, args, args)
}

fn generate_expression(expr: &Expression, depth: usize) -> String {
    match expr {
        Expression::Call(name, args) =>
            format!("(() => {{{}\n{}return {}({})\n{}}})()",
                    args.iter()
                        .filter(|it| matches!(it, Expression::Declare(_,_)))
                        .map(|it| match it {
                            Expression::Declare(_, _) => format!("\n{}{};", "  ".repeat(depth + 1), generate_expression(it, depth + 1)),
                            _ => "NOT POSSIBLE?".to_string()
                        })
                        .collect::<String>(),
                    "  ".repeat(depth + 1),
                    name,
                    args.iter()
                        .map(|it| match it {
                            Expression::Declare(name, _) => generate_expression(&Expression::Variable(name.clone()), depth + 1),
                            _ => generate_expression(it, depth + 1)
                        })
                        .map(|it| format!("{}", it))
                        .collect::<Vec<String>>()
                        .join(", "),
                    "  ".repeat(depth)),
        Expression::Block(block) => {
            if block.len() == 1 {
                match block.first() {
                    Some(expr) => format!("{}", generate_expression(expr, depth)),
                    _ => panic!("Unreachable")
                }
            } else {
                let result = block.iter().enumerate()
                    .filter(|(i, _)| i.clone() < block.len() - 1)
                    .map(|(_, f)| generate_expression(f, depth))
                    .map(|s| format!("\n{}{};", "  ".repeat(depth + 1), s))
                    .collect::<String>();

                match block.last() {
                    Some(Expression::Declare(name, expr)) =>
                        format!("(() => {{{}\n{depth}{}\n{depth}return {};\n{}}})()",
                                result,
                                generate_expression(&Expression::Declare(name.clone(), Box::new(*expr.clone())), depth + 1),
                                name,
                                "  ".repeat(depth),
                                depth = "  ".repeat(depth + 1)),
                    Some(expr) =>
                        format!("(() => {{{}\n{depth}return {};\n{}}})()",
                                result,
                                generate_expression(expr, depth + 1),
                                "  ".repeat(depth),
                                depth = "  ".repeat(depth + 1)),
                    _ => format!(" ")
                }
            }
        }
        Expression::Named(name, expr) => {
            format!("{}", generate_expression(expr, depth))
        }
        Expression::Variable(name) => name.clone(),
        Expression::LiteralNumber(value) => value.clone(),
        Expression::LiteralString(value) => format!("\"{}\"", value.clone()),
        Expression::Wrapped(expr) => format!("({})", generate_expression(expr, depth)),
        Expression::Unary(op, expr) =>
            format!("{}{}",
                    get_unary_op(op),
                    generate_expression(expr, depth)),
        Expression::Binary(lhs, op, rhs) =>
            format!("({} {} {})",
                    generate_expression(lhs, depth),
                    get_binary_op(op),
                    generate_expression(rhs, depth)),
        Expression::If(condition, success, failure) =>
            format!("({} ? {} : {})",
                    generate_expression(condition, depth),
                    generate_expression(success, depth + 1),
                    generate_expression(failure, depth + 1)),
        Expression::Declare(name, expr) =>
            format!("var {} = {}", name, generate_expression(expr, depth))
    }
}

fn get_unary_op(op: &UnaryOp) -> &str {
    match op {
        UnaryOp::Plus => "+",
        UnaryOp::Minus => "-",
        UnaryOp::Not => "!",
    }
}

fn get_binary_op(op: &BinaryOp) -> &str {
    match op {
        BinaryOp::Plus => "+",
        BinaryOp::Minus => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::Lt => "<",
        BinaryOp::Gt => ">",
        BinaryOp::Lte => "<=",
        BinaryOp::Gte => ">=",
        BinaryOp::Equals => "==",
        BinaryOp::NotEquals => "!=",
        BinaryOp::Assign => "=",
        BinaryOp::Dot => ".",
    }
}