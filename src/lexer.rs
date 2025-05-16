use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Whitespace,
    #[regex(r"//.*", logos::skip)] // one line with // or ///
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)] // multiple lines with /**/
    Comment,

    #[regex(r"[0-9]+", |lex| lex.slice().to_string())]
    Number(String),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

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
