use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Null,
    Int,
    String,
    Reference { inner: Box<Type>, mutable: bool },
    Component(String),
    Resource(String),
    Unresolved(String), // when this is a custom type; should be converted into Component or Resource after
    UNDEFINED,
}

impl Type {
    pub fn get_type(token: Token) -> Result<Self, String> {
        match token {
            Token::IntType => Ok(Type::Int),
            Token::StringType => Ok(Type::String),
            Token::Identifier(name) => Ok(Type::Unresolved(name)),
            _ => Err(format!("Token {:?} doesn't represent a Type", token)),
        }
    }
}
