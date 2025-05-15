use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Whitespace,

    #[regex(r"[0-9]+")]
    Number,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    #[token("fn")]
    Fn,
    #[token("let")]
    Let,

    #[token("->")]
    Arrow,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    Assign,

    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token(">")]
    GreaterThan,
    #[token("<")]
    LowerThan,

    #[token("{")]
    CurlyBraceOpen,
    #[token("}")]
    CurlyBraceClose,
    #[token("(")]
    ParenthesisOpen,
    #[token(")")]
    ParenthesisClose,
    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,
}
