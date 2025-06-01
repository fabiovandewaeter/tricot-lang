use std::{env, error::Error, fs};

use interpreter::Interpreter;
use lexer::Token;
use logos::Logos;
use parser::parser::Parser;
use tricot_lang::{interpreter, lexer, parser, types};
use types::type_checker::TypeChecker;

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

    let mut parser = Parser::new(tokens, true);
    let mut program = parser.parse_program();
    let mut type_checker = TypeChecker::new();

    // display AST
    println!("AST :\n{:#?}", program);
    println!("\n===========\n");

    // check types
    let _ = type_checker.check(&mut program);

    // display typed AST
    println!("typed AST :\n{:#?}", program);
    println!("\n===========\n");

    // use the interpreter
    let mut interpreter = Interpreter::new();
    interpreter.run(program);

    Ok(())
}
