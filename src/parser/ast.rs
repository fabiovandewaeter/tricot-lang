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
    Deref, // `*expr`
    /// mutable
    AddrOf(bool), // `&expr`
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
        //name: String,
        target: Expr,
        expression: Expr,
    },
    CompoundAssignment {
        target: Expr,
        op: BinaryOp,
        expression: Expr,
    },
    Function(Function),
    Component(Component),
    Resource(Resource),
    System(System),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Component {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Resource {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Field {
    Named(String, Type),
    Unnamed(Type),
}

#[derive(Debug, Clone)]
pub struct System {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub mutable: bool,
    pub param_type: Type,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
}
