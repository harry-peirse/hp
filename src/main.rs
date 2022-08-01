use std::error::Error;
use std::fs;
use std::fs::{DirBuilder, File};
use std::io::{Read, Write};
use std::path::Path;
use std::process::Command;
use std::str::from_utf8;

use lexer::lex;
use parser::parse;
use interpreter::interpret;
use node_generator::generate_node;

mod lexer;
mod parser;
mod interpreter;
mod node_generator;


fn main() -> Result<(), Box<dyn Error>> {
    fs::create_dir_all("./samples/output")
        .expect("Could not create output directory for node");

    run_pipeline("./samples/fib.hp")?;
    run_pipeline("./samples/ooo.hp")?;
    run_pipeline("./samples/test.hp")?;

    Ok(())
}

fn run_pipeline(filename: &str) -> Result<(), Box<dyn Error>> {
    let mut code = String::new();
    File::open(Path::new(filename))?.read_to_string(&mut code)?;

    println!("Code");
    println!("{}", code);

    let vec = lex(code.to_string())?;

    println!("\nLexemes");
    println!("{}", vec.iter().map(|it| it.to_string()).collect::<Vec<String>>().join("\n"));

    let ast = parse(vec)?;

    println!("\nAST");
    println!("{}", ast.iter().map(|it| it.to_string()).collect::<Vec<String>>().join("\n"));

    println!("\nInterpreting...\n");
    interpret(&ast)?;

    println!("\nNode Target");
    let node_output = generate_node(&ast);
    println!("{}", node_output);

    let output_filename: String = filename.to_string().replace("samples", "samples/output") + ".js";
    println!("{}", output_filename);
    File::create(Path::new(&output_filename))?.write(node_output.as_bytes())?;

    let output = Command::new("node")
        .arg(output_filename)
        .output()
        .expect("failed to execute process");

    let result = from_utf8(&output.stdout)?;

    println!("\nNode Output\n");
    println!("{}", result);

    Ok(())
}