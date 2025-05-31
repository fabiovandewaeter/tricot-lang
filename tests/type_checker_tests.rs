use logos::Logos;
use tricot_lang::{lexer::Token, parser::parser::Parser, types::type_checker::TypeChecker};

fn check(input: &str) {
    let tokens = Token::lexer(input).map(|t| t.unwrap()).collect();
    let mut parser = Parser::new(tokens, false);
    let mut program = parser.parse_program(false);
    let mut type_checker = TypeChecker::new();
    type_checker.check(&mut program);
}

#[test]
fn test_interpreter_variable_declaration() {
    check("let a = 1");
}

#[test]
fn test_interpreter_variable_declaration_string() {
    check("let a = \"word\"");
}

#[test]
fn test_interpreter_assignment() {
    check(
        "
let mut a = 1
a = a + 1",
    );
}

#[test]
#[should_panic(expected = "Type mismatch for 'a': expected 'Int' but found 'String'")]
fn test_interpreter_assignment_with_wrong_type() {
    check(
        "
let mut a = 1
a = \"word\"",
    );
}

#[test]
#[should_panic(expected = "Cannot assign to immutable variable: a")]
fn test_interpreter_assignment_with_immutable_variable() {
    check(
        "
let a = 1
a = 2",
    );
}

#[test]
fn test_interpreter_function_call_with_mutable_reference() {
    check(
        "
fn incr(a: &mut Int) {
    *a = *a + 1
}
let mut a = 1
incr(&mut a)",
    );
}
