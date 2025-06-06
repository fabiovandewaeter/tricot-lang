use logos::Logos;
use tricot_lang::{
    interpreter::interpreter::Interpreter, lexer::Token, parser::parser::Parser,
    types::type_checker::TypeChecker, values::Value,
};

fn run(input: &str) -> Interpreter {
    let tokens = Token::lexer(input).map(|t| t.unwrap()).collect();
    let mut parser = Parser::new(tokens);
    let mut program = parser.parse_program();
    let mut type_checker = TypeChecker::new();
    type_checker.check(&mut program);
    let mut interpreter = Interpreter::new();
    interpreter.run(program);
    interpreter
}

#[test]
fn test_interpreter_variable_declaration() {
    let interpreter = run("let a = 1");

    assert_eq!(interpreter.get_variable("a"), Value::Int(1));
}

#[test]
fn test_interpreter_variable_declaration_string() {
    let interpreter = run("let a = \"word\"");

    assert_eq!(
        interpreter.get_variable("a"),
        Value::String("word".to_string())
    );
}

#[test]
fn test_interpreter_assignment() {
    let interpreter = run("
let mut a = 1
a = a + 1");

    assert_eq!(interpreter.get_variable("a"), Value::Int(2));
}

#[test]
fn test_interpreter_function_call_with_mutable_reference() {
    let interpreter = run("
fn incr(a: &mut Int) {
    *a = *a + 1
}
let mut a = 1
incr(&mut a)");

    assert_eq!(interpreter.get_variable("a"), Value::Int(2));
}

// ECS
#[test]
fn test_interpreter_system_with_implicit_loop() {
    let interpreter = run("
comp Position(x: Int, y: Int)
comp Velocity(dx: Int, dy: Int)
comp Health(Int)

res Time(Int)

sys move_entities(position: mut Position, velocity: Velocity) using (time: Time) {
    position.x += velocity.dx
    position.y += velocity.dy

    save_variable_for_tests(\"tempo_x\", position.x)
    save_variable_for_tests(\"tempo_y\", position.y)
}

schedule {
    move_entities
}
");

    assert_eq!(
        interpreter.get_saved_for_tests("tempo_x".into()),
        Value::Int(2)
    );
    assert_eq!(
        interpreter.get_saved_for_tests("tempo_y".into()),
        Value::Int(2)
    );
}
