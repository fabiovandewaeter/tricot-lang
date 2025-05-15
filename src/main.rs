use std::{env, error::Error, fs};

use lexer::Token;
use logos::Logos;

mod lexer;

fn main() -> Result<(), Box<dyn Error>> {
    let path = match env::args().nth(1) {
        Some(p) => p,
        None => {
            eprintln!("Usage: lexer <source-file>");
            return Ok(());
        }
    };

    let source_code = fs::read_to_string(&path)?;

    let mut lexer = Token::lexer(&source_code);

    while let Some(token) = lexer.next() {
        match token {
            Ok(tok) => println!("Token: {:?} (span: {:?})", tok, lexer.span()),
            Err(_) => println!("Erreur de lexing à {:?}", lexer.span()),
        }
    }

    Ok(())
}
