use logos::Logos;
use tricot_lang::lexer::Token;

#[test]
fn test_lexer_basic() {
    let input = "let mut x = 42;";
    let mut lexer = Token::lexer(input);

    assert_eq!(lexer.next(), Some(Ok(Token::Let)));
    assert_eq!(lexer.next(), Some(Ok(Token::Mut)));
    assert_eq!(lexer.next(), Some(Ok(Token::Identifier("x".into()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Assign)));
    assert_eq!(lexer.next(), Some(Ok(Token::Number("42".into()))));
    assert_eq!(lexer.next(), Some(Ok(Token::Semicolon)));
}
