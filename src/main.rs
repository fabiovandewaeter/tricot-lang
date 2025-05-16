use std::{env, error::Error, fs};

use interpreter::interpreter::Interpreter;
use lexer::Token;
use logos::{Lexer, Logos};
use parser::{ast::Program, parser::Parser};

mod interpreter;
mod lexer;
mod parser;

fn main() -> Result<(), Box<dyn Error>> {
    let path = match env::args().nth(1) {
        Some(p) => p,
        None => {
            eprintln!("Usage: lexer <source-file>");
            return Ok(());
        }
    };

    let source_code = fs::read_to_string(&path)?;

    let lexer = Token::lexer(&source_code);
    //print_tokens(&mut lexer);
    let tokens: Vec<Token> = lexer.map(|r| r.expect("Erreur de lexing")).collect();
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program();
    match program {
        Some(prog) => {
            println!("AST :\n{:#?}", prog);
            let mut interpreter = Interpreter::new();
            interpreter.run(prog);
        }
        None => {
            eprintln!("Error during parsing");
        }
    }

    Ok(())
}

fn print_tokens(lexer: &mut Lexer<'_, Token>) {
    while let Some(token) = lexer.next() {
        match token {
            Ok(tok) => println!("Token: {:?} (span: {:?})", tok, lexer.span()),
            Err(_) => println!("Erreur de lexing à {:?}", lexer.span()),
        }
    }
}
