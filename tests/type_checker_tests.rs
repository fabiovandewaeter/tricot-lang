use logos::Logos;
use tricot_lang::{lexer::Token, parser::parser::Parser, types::type_checker::TypeChecker};

fn check(input: &str) {
    let tokens = Token::lexer(input).map(|t| t.unwrap()).collect();
    let mut parser = Parser::new(tokens);
    let mut program = parser.parse_program();
    let mut type_checker = TypeChecker::new();
    type_checker.check(&mut program);
}

#[test]
fn test_type_checker_variable_declaration() {
    check("let a = 1");
}

#[test]
fn test_type_checker_variable_declaration_string() {
    check("let a = \"word\"");
}

#[test]
fn test_type_checker_assignment() {
    check(
        "
let mut a = 1
a = a + 1",
    );
}

#[test]
#[should_panic(expected = "Assignment type mismatch: expected Int, found String")]
fn test_type_checker_assignment_with_wrong_type() {
    check(
        "
let mut a = 1
a = \"word\"",
    );
}

#[test]
#[should_panic(expected = "Cannot assign to immutable expression: Identifier(\"a\")")]
fn test_type_checker_assignment_with_immutable_variable() {
    check(
        "
let a = 1
a = 2",
    );
}

#[test]
fn test_type_checker_function_call_with_mutable_reference() {
    check(
        "
fn incr(a: &mut Int) {
    *a += 1
}
let mut a = 1
incr(&mut a)",
    );
}

// ECS
#[test]
fn test_type_checker_components_in_system() {
    check(
        "
comp Position(x: Int, y: Int)
comp Velocity(dx: Int, dy: Int)

sys move_entities(position: mut Position, velocity: Velocity) {
    position.x += velocity.dx
    position.y += velocity.dy
}",
    );
}

#[test]
fn test_type_checker_resource_in_system() {
    check(
        "
res Time(Int)

sys move_entities() using (time: Time) {
    print(time.0)
}",
    );
}

#[test]
fn test_type_checker_system() {
    check(
        "
comp Position(x: Int, y: Int)
comp Velocity(dx: Int, dy: Int)

res Time(Int)

sys move_entities(position: mut Position, velocity: Velocity) using (time: Time) {
    position.x += velocity.dx
    position.y += velocity.dy
    print(time.0)
}",
    );
}

#[test]
#[should_panic(expected = "Type mismatch in compound assignment: expected String, found Int")]
fn test_type_checker_panic_when_wrong_component_field_type() {
    check(
        "
comp Position(x: Int, y: String)
comp Velocity(dx: Int, dy: Int)

sys move_entities(position: mut Position, velocity: Velocity)  {
    position.x += velocity.dx
    position.y += velocity.dy
}",
    );
}
