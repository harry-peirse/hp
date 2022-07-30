use crate::parser::{BinaryOp, Expression, Function, UnaryOp};

pub fn generate_node(ast: &Vec<Function>) -> String {
    let functions = ast.iter().map(generate_function).collect::<Vec<String>>().join("\n\n");
    format!("{}\n\nmain();

function print(arg) {{
  console.log(arg);
}}", functions)
}

fn generate_function(fun: &Function) -> String {
    let args = fun.args.iter().map(|it| it.name.clone()).collect::<Vec<String>>().join(", ");
    format!("function {}({}) {{{}}}", fun.name, args, generate_block(&fun.block, 1))
}

fn generate_block(block: &Vec<Expression>, depth: usize) -> String {
    let result = block.iter().enumerate()
        .filter(|(i, _)| i.clone() < block.len() - 1)
        .map(|(i, f)| generate_expression(f, depth))
        .map(|s| format!("\n{}{};", "  ".repeat(depth), s))
        .collect::<String>();

    match block.last() {
        Some(Expression::Declare(name, expr)) =>
            format!("{}\n{}{}\n{}return {};\n",
                    result,
                    "  ".repeat(depth),
                    generate_expression(&Expression::Declare(name.clone(), Box::new(*expr.clone())), depth),
                    "  ".repeat(depth),
                    name),
        Some(expr) => format!("{}\n{}return {};\n", result, "  ".repeat(depth), generate_expression(expr, depth)),
        _ => format!(" ")
    }
}

fn generate_expression(expr: &Expression, depth: usize) -> String {
    match expr {
        Expression::Call(name, args) => format!("{}({})", name, args.iter()
            .map(|it| generate_block(&vec!(it.clone()), depth + 1))
            .map(|it| format!("(() => {{{}}})()", it))
            .collect::<String>()),
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
            format!("({}) ? (() => {{{}{depth}}})() : (() => {{{}{depth}}})()",
                    generate_expression(condition, depth),
                    generate_block(success, depth + 1),
                    generate_block(failure, depth + 1),
                    depth = "  ".repeat(depth)),
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