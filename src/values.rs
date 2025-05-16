#[derive(Debug, Clone)]
pub enum Value {
    Null,
    Int(i64),
    String(String),
}
