use std::{env, error::Error, fs};

use lexer::Token;
use logos::{Lexer, Logos};
use parser::parser::Parser;

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
    print_ast(&mut parser);

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

fn print_ast(parser: &mut Parser) {
    match parser.parse_program() {
        Some(prog) => {
            println!("AST complet :\n{:#?}", prog);
            // ici tu peux appeler ton interpréteur ou générateur de code
        }
        None => {
            eprintln!("Échec du parsing, programme invalide.");
        }
    }
}
