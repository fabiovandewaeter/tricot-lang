use crate::{lexer::Token, types::types::Type};

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    StringLiteral(String),
    Identifier(String),
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>, // an Identifier
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Deref,  // `*expr`
    AddrOf, // `&expr`
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
}

impl BinaryOp {
    pub fn get_binary_op(token: &Token) -> Result<BinaryOp, String> {
        match token {
            Token::Plus => Ok(BinaryOp::Plus),
            Token::Minus => Ok(BinaryOp::Minus),
            Token::Star => Ok(BinaryOp::Star),
            Token::Slash => Ok(BinaryOp::Slash),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Let {
        mutable: bool,
        name: String,
        expected_type: Type,
        expression: Expr,
    },
    Assignment {
        name: String,
        expression: Expr,
    },
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
}
