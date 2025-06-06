use std::collections::HashMap;

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
    Reference {
        name: String,
        mutable: bool,
        depth_at_creation: usize,
    },
    ComponentInstance {
        name: String,
        fields: HashMap<String, Value>,
    },
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
