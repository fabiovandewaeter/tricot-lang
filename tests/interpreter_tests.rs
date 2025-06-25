use logos::Logos;
use tricot_lang::{
    interpreter::interpreter::Interpreter, lexer::Token, parser::parser::Parser,
    types::type_checker::TypeChecker, values::Value,
};

fn run(input: &str, scheduler_loops: i32) -> Interpreter {
    let tokens = Token::lexer(input).map(|t| t.unwrap()).collect();
    let mut parser = Parser::new(tokens);
    let mut program = parser.parse_program();
    let mut type_checker = TypeChecker::new();
    type_checker.check(&mut program);
    let mut interpreter = Interpreter::new();
    interpreter.run(program, scheduler_loops);
    interpreter
}

#[test]
fn test_interpreter_variable_declaration() {
    let interpreter = run("let a = 1", 1);

    assert_eq!(interpreter.get_variable("a"), Value::Int(1));
}

#[test]
fn test_interpreter_variable_declaration_string() {
    let interpreter = run("let a = \"word\"", 1);

    assert_eq!(
        interpreter.get_variable("a"),
        Value::String("word".to_string())
    );
}

#[test]
fn test_interpreter_assignment() {
    let interpreter = run(
        "
let mut a = 1
a = a + 1",
        1,
    );

    assert_eq!(interpreter.get_variable("a"), Value::Int(2));
}

#[test]
fn test_interpreter_function_call_with_mutable_reference() {
    let interpreter = run(
        "
fn incr(a: &mut Int) {
    *a = *a + 1
}
let mut a = 1
incr(&mut a)",
        1,
    );

    assert_eq!(interpreter.get_variable("a"), Value::Int(2));
}

// ECS
#[test]
fn test_interpreter_system_with_implicit_loop() {
    let interpreter = run(
        "
comp Position(x: Int, y: Int)
comp Velocity(dx: Int, dy: Int)
comp Health(Int)

res SpawnCount(Int)

sys move_entities(position: mut Position, velocity: Velocity) {
    position.x += velocity.dx
    position.y += velocity.dy

    save_variable_for_tests(\"tempo_x\", position.x)
    save_variable_for_tests(\"tempo_y\", position.y)
}

sys entity_spawner() using (spawn_count: mut SpawnCount) {
    let id = spawn {
        Position(0, 0),
        Velocity(1, 1)
    }

    spawn_count = spawn_count + 1

    save_variable_for_tests(\"entity_spawned\", spawn_count)
}

setup {
    SpawnCount {0},
}

schedule {
    once(entity_spawner),
    move_entities
}
",
        2,
    );

    // 2 because runned 2 times
    assert_eq!(
        interpreter.get_saved_for_tests("tempo_x".into()),
        Value::Int(2)
    );
    assert_eq!(
        interpreter.get_saved_for_tests("tempo_y".into()),
        Value::Int(2)
    );
    // checks that only once entity is spawned because of once()
    assert_eq!(
        interpreter.get_saved_for_tests("entity_spawned".into()),
        Value::Int(1)
    );
}
