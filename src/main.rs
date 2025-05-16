use std::{env, error::Error, fs};

use interpreter::interpreter::Interpreter;
use lexer::Token;
use logos::{Lexer, Logos};
use parser::parser::Parser;

mod interpreter;
mod lexer;
mod parser;
mod types;
mod values;

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
    let tokens_with_span: Vec<(Token, std::ops::Range<usize>)> = lexer
        .spanned()
        .map(|(tok, span)| (tok.expect("Lexing error"), span))
        .collect();

    println!("Tokens:");
    for (tok, span) in &tokens_with_span {
        println!("{:?} (span: {:?})", tok, span);
    }
    println!("\n===========\n");

    let tokens: Vec<Token> = tokens_with_span.into_iter().map(|(tok, _)| tok).collect();

    let mut parser = Parser::new(tokens);
    let program = parser.parse_program();

    match program {
        Some(prog) => {
            println!("AST :\n{:#?}", prog);
            println!("\n===========\n");
            let mut interpreter = Interpreter::new();
            interpreter.run(prog);
        }
        None => {
            eprintln!("Error during parsing");
        }
    }

    Ok(())
}
