use crate::lexer::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    Identifier(String),
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Let { name: String, value: Expr },
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Stmt>,
}
