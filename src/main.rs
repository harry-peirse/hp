use std::error::Error;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use lexer::lex;

mod lexer;

fn main() -> Result<(), Box<dyn Error>> {
    let mut code = String::new();
    File::open(Path::new("./samples/fib.hp"))?.read_to_string(&mut code)?;

    println!("Code");
    println!("{}", code);

    let vec = lex(code.to_string());

    println!("\nLexemes");
    println!("{}", vec.iter().map(|it| format!("{}", it)).collect::<Vec<String>>().join("\n"));

    Ok(())
}
