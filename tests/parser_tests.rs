use logos::Logos;
use tricot_lang::{
    lexer::Token,
    parser::{
        ast::{Expr, Stmt},
        parser::Parser,
    },
    types::types::Type,
};

fn parse(input: &str) -> Vec<Stmt> {
    let tokens = Token::lexer(input).map(|t| t.unwrap()).collect();
    let mut parser = Parser::new(tokens, false);
    parser.parse_program(false).statements
}

#[test]
fn test_parse_variable_declaration() {
    let statements = parse("let mut x = 42");

    assert_eq!(statements.len(), 1,);

    let Stmt::Let {
        mutable,
        name,
        expected_type,
        expression,
    } = &statements[0]
    else {
        panic!("Should have been a Stmt::Let {:?}", statements[0]);
    };

    assert!(mutable);
    assert_eq!(name, "x");

    let Expr::Number(value) = *expression else {
        panic!("Should have been a Expr::Number {:?}", expression);
    };

    assert_eq!(value, 42);
}

#[test]
fn test_parse_function_declaration() {
    let statements = parse(
        "fn incr(a: &mut Int) -> Int {
    *a = *a + 1
    0
}",
    );

    assert_eq!(statements.len(), 1,);

    let Stmt::Function(function) = &statements[0] else {
        panic!("Should have been a Stmt::Function {:?}", statements[0]);
    };

    assert_eq!(function.name, "incr");

    assert_eq!(
        function.params[0],
        (
            "a".to_string(),
            Type::Reference {
                inner: Box::new(Type::Int),
                mutable: true
            }
        )
    );

    let Type::Int = function.return_type else {
        panic!("Should have been a Type::Int {:?}", function.return_type);
    };
}
