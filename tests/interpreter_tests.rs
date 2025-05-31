use logos::Logos;
use tricot_lang::{
    interpreter::interpreter::Interpreter, lexer::Token, parser::parser::Parser,
    types::type_checker::TypeChecker, values::Value,
};

fn run(input: &str) -> Interpreter {
    let tokens = Token::lexer(input).map(|t| t.unwrap()).collect();
    let mut parser = Parser::new(tokens, false);
    let mut program = parser.parse_program(false);
    let mut type_checker = TypeChecker::new();
    type_checker.check(&mut program);
    let mut interpreter = Interpreter::new();
    interpreter.run(program);
    interpreter
}

#[test]
fn test_interpreter_variable_declaration() {
    let interpreter = run("let a = 1");

    assert_eq!(interpreter.lookup("a"), Value::Int(1));
}

#[test]
fn test_interpreter_variable_declaration_string() {
    let interpreter = run("let a = \"word\"");

    assert_eq!(interpreter.lookup("a"), Value::String("word".to_string()));
}

#[test]
fn test_interpreter_assignment() {
    let interpreter = run("
let mut a = 1
a = a + 1");

    assert_eq!(interpreter.lookup("a"), Value::Int(2));
}

#[test]
fn test_interpreter_function_call_with_mutable_reference() {
    let interpreter = run("
fn incr(a: &mut Int) {
    *a = *a + 1
}
let mut a = 1
incr(&mut a)");

    assert_eq!(interpreter.lookup("a"), Value::Int(2));
}
