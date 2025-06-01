use logos::Logos;
use tricot_lang::{
    lexer::Token,
    parser::{
        ast::{Expr, Field, Stmt},
        parser::Parser,
    },
    types::types::Type,
};

fn parse(input: &str) -> Vec<Stmt> {
    let tokens = Token::lexer(input).map(|t| t.unwrap()).collect();
    let mut parser = Parser::new(tokens, false);
    parser.parse_program().statements
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

    assert_eq!(statements.len(), 1,);

    let Stmt::Function(function) = &statements[0] else {
        panic!("Should have been a Stmt::Function : {:?}", statements[0]);
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
        panic!("Should have been a Type::Int : {:?}", function.return_type);
    };
}

// ECS
#[test]
fn test_parse_component_declaration_with_unnamed_fields() {
    let statements = parse("comp Example(String, Int)");

    assert_eq!(statements.len(), 1,);

    let Stmt::ComponentDeclaration(component_declaration) = &statements[0] else {
        panic!(
            "Should have been a Stmt::ComponentDeclaration : {:?}",
            statements[0]
        );
    };

    assert_eq!(component_declaration.name, "Example");

    assert_eq!(
        component_declaration.fields[0],
        Field::Unnamed(Type::String)
    );
    assert_eq!(component_declaration.fields[1], Field::Unnamed(Type::Int));
}

#[test]
fn test_parse_component_declaration_with_named_fields() {
    let statements = parse("comp Position(x: Int, y: Int)");

    assert_eq!(statements.len(), 1,);

    let Stmt::ComponentDeclaration(component_declaration) = &statements[0] else {
        panic!(
            "Should have been a Stmt::ComponentDeclaration : {:?}",
            statements[0]
        );
    };

    assert_eq!(component_declaration.name, "Position");

    assert_eq!(
        component_declaration.fields[0],
        Field::Named("x".to_string(), Type::Int)
    );
    assert_eq!(
        component_declaration.fields[1],
        Field::Named("y".to_string(), Type::Int)
    );
}

#[test]
fn test_parse_resource_declaration_with_unnamed_fields() {
    let statements = parse("res Resource(String, Int)");

    assert_eq!(statements.len(), 1,);

    let Stmt::ResourceDeclaration(resource_declaration) = &statements[0] else {
        panic!(
            "Should have been a Stmt::ResourceDeclaration : {:?}",
            statements[0]
        );
    };

    assert_eq!(resource_declaration.name, "Resource");

    assert_eq!(resource_declaration.fields[0], Field::Unnamed(Type::String));
    assert_eq!(resource_declaration.fields[1], Field::Unnamed(Type::Int));
}

#[test]
fn test_parse_resource_declaration_with_named_fields() {
    let statements = parse("res Resource(x: Int, y: Int)");

    assert_eq!(statements.len(), 1,);

    let Stmt::ResourceDeclaration(resource_declaration) = &statements[0] else {
        panic!(
            "Should have been a Stmt::ResourceDeclaration : {:?}",
            statements[0]
        );
    };

    assert_eq!(resource_declaration.name, "Resource");

    assert_eq!(
        resource_declaration.fields[0],
        Field::Named("x".to_string(), Type::Int)
    );
    assert_eq!(
        resource_declaration.fields[1],
        Field::Named("y".to_string(), Type::Int)
    );
}
