use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::fs::{DirBuilder, File};
use std::io::{Read, Write};
use std::iter::Map;
use std::path::Path;
use std::process::Command;
use std::rc::Rc;
use std::str::from_utf8;

use lexer::lex;
use node_generator::generate_node;
use parser::parse;

use crate::parser::{Declaration, Function, Struct};
use crate::type_checker::type_check;

mod lexer;
mod parser;
mod type_checker;
mod node_generator;

#[feature(exclusive_range_pattern)]
fn main() -> Result<(), Box<dyn Error>> {
    fs::create_dir_all("./samples/output")
        .expect("Could not create output directory for node");

    // run_pipeline("./samples/hello_world.hp")?;
    // run_pipeline("./samples/greeter.hp")?;
    // run_pipeline("./samples/counter.hp")?;
    run_pipeline("self_compiler")?;
    Ok(())
}

fn run_pipeline(file_name: &str) -> Result<(), Box<dyn Error>> {
    let mut project = compile_to_project(file_name.to_string())?;

    println!("Created project {:#?}", project);

    type_check(&mut project)?;

    println!("Type-checked project {:#?}", project);

    println!("\nNode Target");
    generate_node(&project)?;

    let output = Command::new("node")
        .arg(format!("./samples/output/{}", file_name))
        .output()
        .expect("failed to execute process");

    let result = from_utf8(&output.stdout)?;

    println!("\nNode Output\n");
    println!("{}", result);

    Ok(())
}

fn compile_to_project(file_name: String) -> Result<Project, Box<dyn Error>> {
    let mut unresolved_modules = vec!();
    let mut module_map = HashMap::new();
    unresolved_modules.push(file_name.clone());
    while !unresolved_modules.is_empty() {
        let module_name = unresolved_modules.remove(0);
        let mut module = parse_module(&module_name)?;
        for import in &module.module_imports {
            if let None = module_map.get(&import.clone()) {
                if !unresolved_modules.contains(&import) {
                    unresolved_modules.push(import.clone())
                }
            }
        }
        module_map.insert(module_name.clone(), module);
    }
    Ok(Project { modules: module_map.into_values().collect::<Vec<Module>>() })
}

fn parse_module(file_name: &String) -> Result<Module, Box<dyn Error>> {
    let mut code = String::new();
    File::open(Path::new(&format!("./samples/{}.hp", file_name)))?.read_to_string(&mut code)?;

    let mut module = Module {
        name: file_name.to_string(),
        module_imports: vec!(),
        functions: vec!(),
        structs: vec!(),
    };

    let lexemes = lex(code)?;
    let declarations = parse(lexemes)?;
    for dec in declarations {
        match dec {
            Declaration::Import(it) => module.module_imports.push(it),
            Declaration::Function(it) => module.functions.push(it),
            Declaration::Struct(it) => module.structs.push(it),
        }
    }

    Ok(module)
}

#[derive(Debug)]
pub struct Project {
    pub modules: Vec<Module>,
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub module_imports: Vec<String>,
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
}