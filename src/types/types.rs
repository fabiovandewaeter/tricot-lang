use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Null,
    Int,
    String,
    Reference { inner: Box<Type>, mutable: bool },
    Component(String),
    Resource(String),
    Struct(String),
    Unresolved(String), // when this is a custom type; should be converted into Component or Resource after
    UNDEFINED,
}

/// when parse parameters and find a Token::Identifier, return a Type::Unresolved(), but in parse_parameters() we can add ParamContext to know if it's a Component, a Resource or a Struct
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamContext {
    Component,
    Resource,
    Struct,
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
