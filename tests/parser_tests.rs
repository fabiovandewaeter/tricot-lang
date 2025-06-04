use logos::Logos;
use tricot_lang::{
    lexer::Token,
    parser::{
        ast::{BinaryOp, Expr, Field, Param, Stmt},
        parser::Parser,
    },
    types::types::Type,
};

fn parse(input: &str) -> Vec<Stmt> {
    let tokens = Token::lexer(input).map(|t| t.unwrap()).collect();
    let mut parser = Parser::new(tokens);
    parser.parse_program().statements
}

#[test]
fn test_parse_variable_declaration() {
    let statements = parse("let mut x = 42");

    assert_eq!(statements.len(), 1);

    let Stmt::Let {
        mutable,
        name,
        expected_type,
        expression,
    } = &statements[0]
    else {
        panic!("Should have been a Stmt::Let : {:?}", statements[0]);
    };

    assert!(mutable);
    assert_eq!(name, "x");
    assert_eq!(*expected_type, Type::UNDEFINED);

    let Expr::Number(value) = *expression else {
        panic!("Should have been a Expr::Number : {:?}", expression);
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

    assert_eq!(statements.len(), 1);

    let Stmt::Function(function) = &statements[0] else {
        panic!("Should have been a Stmt::Function : {:?}", statements[0]);
    };

    assert_eq!(function.name, "incr");

    assert_eq!(
        function.params[0],
        Param {
            name: "a".to_string(),
            mutable: false,
            param_type: Type::Reference {
                inner: Box::new(Type::Int),
                mutable: true
            }
        }
    );

    let Type::Int = function.return_type else {
        panic!("Should have been a Type::Int : {:?}", function.return_type);
    };
}

// ECS
#[test]
fn test_parse_component_declaration_with_unnamed_fields() {
    let statements = parse("comp Example(String, Int)");

    assert_eq!(statements.len(), 1);

    let Stmt::Component(component) = &statements[0] else {
        panic!("Should have been a Stmt::Component: {:?}", statements[0]);
    };

    assert_eq!(component.name, "Example");

    assert_eq!(component.fields[0], Field::Unnamed(Type::String));
    assert_eq!(component.fields[1], Field::Unnamed(Type::Int));
}

#[test]
fn test_parse_component_declaration_with_named_fields() {
    let statements = parse("comp Position(x: Int, y: Int)");

    assert_eq!(statements.len(), 1);

    let Stmt::Component(component) = &statements[0] else {
        panic!("Should have been a Stmt::Component: {:?}", statements[0]);
    };

    assert_eq!(component.name, "Position");

    assert_eq!(
        component.fields[0],
        Field::Named("x".to_string(), Type::Int)
    );
    assert_eq!(
        component.fields[1],
        Field::Named("y".to_string(), Type::Int)
    );
}

#[test]
fn test_parse_resource_declaration_with_unnamed_fields() {
    let statements = parse("res Resource(String, Int)");

    assert_eq!(statements.len(), 1);

    let Stmt::Resource(resource) = &statements[0] else {
        panic!(
            "Should have been a Stmt::ResourceDeclaration : {:?}",
            statements[0]
        );
    };

    assert_eq!(resource.name, "Resource");

    assert_eq!(resource.fields[0], Field::Unnamed(Type::String));
    assert_eq!(resource.fields[1], Field::Unnamed(Type::Int));
}

#[test]
fn test_parse_resource_declaration_with_named_fields() {
    let statements = parse("res Resource(x: Int, y: Int)");

    assert_eq!(statements.len(), 1);

    let Stmt::Resource(resource) = &statements[0] else {
        panic!(
            "Should have been a Stmt::ResourceDeclaration : {:?}",
            statements[0]
        );
    };

    assert_eq!(resource.name, "Resource");

    assert_eq!(resource.fields[0], Field::Named("x".to_string(), Type::Int));
    assert_eq!(resource.fields[1], Field::Named("y".to_string(), Type::Int));
}

#[test]
fn test_parse_system_with_implicit_loop() {
    let statements = parse(
        "comp Position(x: Int, y: Int)
comp Velocity(dx: Int, dy: Int)
sys move_entities(position: mut Position, velocity: Velocity) {
    position.x += velocity.dx
    position.y += velocity.dy
}",
    );

    assert_eq!(statements.len(), 5);

    let Stmt::System(system) = &statements[2] else {
        panic!("Should have been a Stmt::System : {:?}", statements[0]);
    };

    assert_eq!(system.name, "move_entities");

    assert_eq!(
        system.params[0],
        Param {
            name: "Position".to_string(),
            mutable: true,
            param_type: Type::Component("Position".into())
        }
    );
    assert_eq!(
        system.params[1],
        Param {
            name: "Velocity".to_string(),
            mutable: false,
            param_type: Type::Component("Velocity".into())
        }
    );
}

#[test]
fn test_parse_components_dot_operator() {
    let statements = parse(
        "comp Position(x: Int, y: Int)

sys example(position: Position) {
    print(position.x)
    print(position.y)
}",
    );

    assert_eq!(statements.len(), 2);

    let Stmt::System(system) = &statements[1] else {
        panic!("Should have been a Stmt::System: {:?}", statements[1]);
    };

    assert_eq!(system.name, "example");

    assert_eq!(
        system.params[0],
        Param {
            name: "position".to_string(),
            mutable: false,
            param_type: Type::Component("Position".into()),
        }
    );

    assert_eq!(
        system.body[0],
        Stmt::Expr(Expr::Call {
            callee: Box::new(Expr::Identifier("print".into())),
            args: vec![
                (Expr::BinaryOp {
                    left: Box::new(Expr::Identifier("position".into())),
                    op: BinaryOp::Dot,
                    right: Box::new(Expr::Identifier("x".into()))
                })
            ],
        })
    );

    assert_eq!(
        system.body[1],
        Stmt::Expr(Expr::Call {
            callee: Box::new(Expr::Identifier("print".into())),
            args: vec![
                (Expr::BinaryOp {
                    left: Box::new(Expr::Identifier("position".into())),
                    op: BinaryOp::Dot,
                    right: Box::new(Expr::Identifier("y".into()))
                })
            ],
        })
    );
}
