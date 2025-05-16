use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Null,
    Int,
    String,
}

impl Type {
    pub fn get_type(token: Token) -> Result<Self, String> {
        match token {
            Token::IntType => Ok(Type::Int),
            Token::StringType => Ok(Type::String),
            _ => Err(format!("Token {:?} doesn't represent a Type", token)),
        }
    }
}
