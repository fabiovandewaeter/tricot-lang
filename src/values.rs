use crate::types::types::Type;

#[derive(Debug, Clone)]
pub struct VarInfo {
    pub value: Value,
    pub mutable: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Int(i64),
    String(String),
    Reference { name: String, mutable: bool },
}

impl Value {
    pub fn get_type(&self) -> Type {
        match *self {
            Value::Null => Type::Null,
            Value::Int(_) => Type::Int,
            Value::String(_) => Type::String,
            _ => unreachable!(),
        }
    }
}
