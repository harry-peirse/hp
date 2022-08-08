use std::error::Error;
use std::fs::File;
use std::io::Write;
use std::path::Path;

use crate::parser::{BinaryOp, Declaration, Expression, Function, Struct, UnaryOp};
use crate::Project;

pub fn generate_node(project: &Project) -> Result<(), Box<dyn Error>> {
    for module in project.modules.iter() {
        let imports = module.module_imports.iter()
            .map(|it| format!("{name} = require('{name}');", name = it))
            .collect::<Vec<String>>()
            .join("\n");
        let functions = module.functions.iter()
            .map(|it| generate_function(it))
            .collect::<Vec<String>>()
            .join("\n\n");
        let file_contents = format!("$readline = require('readline');
$fs = require('fs');

{}

{}

main();

async function write(arg) {{
  console.log(arg);
  return arg;
}}

async function read() {{
    const rl = $readline.createInterface(process.stdin);
    const result = await new Promise((res) => rl.question('', res));
    rl.close();
    return result;
}}

async function read_file(filename) {{
    return $fs.readFileSync(filename).toString();
}}
", imports, functions);

        File::create(Path::new(&format!("./samples/output/{}.js", module.name)))?.write(file_contents.as_bytes())?;
    }
    Ok(())
}

fn generate_function(fun: &Function) -> String {
    let args = fun.args.iter().map(|it| it.name.clone()).collect::<Vec<String>>().join(", ");
    format!("async function {}({}) {{
  return {};
}}", fun.name, args, generate_expression(&fun.body, 1))
}

fn generate_struct(str: &Struct) -> String {
    let args = str.fields.iter().map(|it| it.name.clone()).collect::<Vec<String>>().join(", ");
    format!("async function {}({}) {{
  return {{{}}};
}}", str.name, args, args)
}

fn generate_expression(expr: &Expression, depth: usize) -> String {
    match expr {
        Expression::Call(_, name, args) =>
            format!("(await {}({}))",
                    name,
                    args.iter()
                        .map(|it| generate_expression(it, depth + 1))
                        .collect::<Vec<String>>()
                        .join(", ")),
        Expression::Block(_, block) => {
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
                    Some(expr) =>
                        format!("(await (async () => {{{}\n{depth}return {};\n{}}})())",
                                result,
                                generate_expression(expr, depth + 1),
                                "  ".repeat(depth),
                                depth = "  ".repeat(depth + 1)),
                    _ => format!(" ")
                }
            }
        }
        Expression::Variable(_, name) => name.clone(),
        Expression::LiteralNumber(value) => value.clone(),
        Expression::LiteralString(value) => format!("\"{}\"", value.clone()),
        Expression::Wrapped(_, expr) => format!("({})", generate_expression(expr, depth)),
        Expression::Unary(_, op, expr) =>
            format!("{}{}",
                    get_unary_op(op),
                    generate_expression(expr, depth)),
        Expression::Binary(_, lhs, op, rhs) =>
            format!("({} {} {})",
                    generate_expression(lhs, depth),
                    get_binary_op(op),
                    generate_expression(rhs, depth)),
        Expression::If(_, condition, success, failure) =>
            format!("({} ? {} : {})",
                    generate_expression(condition, depth),
                    generate_expression(success, depth + 1),
                    generate_expression(failure, depth + 1)),
        Expression::Loop(count, body) =>
            format!("for (let $i = 0; $i < {}; $i++) {}",
                    generate_expression(count, depth),
                    generate_expression(body, depth + 1)),
        Expression::Declare(name, expr) =>
            format!("let {} = {}", name, generate_expression(expr, depth))
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
        BinaryOp::PlusAssign => "+=",
        BinaryOp::MinusAssign => "-=",
        BinaryOp::MultiplyAssign => "*=",
        BinaryOp::DivideAssign => "/=",
        BinaryOp::Dot => ".",
    }
}