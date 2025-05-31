use logos::Logos;
use tricot_lang::lexer::Token;

#[test]
fn test_lexer_basic() {
    let input = "let mut x = 42";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::Let)));
    assert_eq!(lexer.next(), Some(Ok(Token::Mut)));
    assert_eq!(lexer.next(), Some(Ok(Token::Identifier("x".into()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
    assert_eq!(lexer.next(), Some(Ok(Token::Number("42".into()))));
}

#[test]
fn test_lexer_function_definition() {
    let input = "fn add(a: Int, b: Int) -> Int {
        a + b
    }";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::Fn)));
    assert_eq!(lexer.next(), Some(Ok(Token::Identifier("add".into()))));
    assert_eq!(lexer.next(), Some(Ok(Token::ParenthesisOpen)));
    assert_eq!(lexer.next(), Some(Ok(Token::Identifier("a".into()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Colon)));
    assert_eq!(lexer.next(), Some(Ok(Token::IntType)));
    assert_eq!(lexer.next(), Some(Ok(Token::Comma)));
    assert_eq!(lexer.next(), Some(Ok(Token::Identifier("b".into()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Colon)));
    assert_eq!(lexer.next(), Some(Ok(Token::IntType)));
    assert_eq!(lexer.next(), Some(Ok(Token::ParenthesisClose)));
    assert_eq!(lexer.next(), Some(Ok(Token::Arrow)));
    assert_eq!(lexer.next(), Some(Ok(Token::IntType)));
    assert_eq!(lexer.next(), Some(Ok(Token::CurlyBraceOpen)));
    assert_eq!(lexer.next(), Some(Ok(Token::Identifier("a".into()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Plus)));
    assert_eq!(lexer.next(), Some(Ok(Token::Identifier("b".into()))));
    assert_eq!(lexer.next(), Some(Ok(Token::CurlyBraceClose)));
}
