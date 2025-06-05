use crate::{lexer::Token, types::types::Type, values::Value};

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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
    Dot,
}

impl BinaryOp {
    pub fn get_binary_op(token: &Token) -> Result<BinaryOp, String> {
        match token {
            Token::Plus => Ok(BinaryOp::Plus),
            Token::Minus => Ok(BinaryOp::Minus),
            Token::Star => Ok(BinaryOp::Star),
            Token::Slash => Ok(BinaryOp::Slash),
            Token::Dot => Ok(BinaryOp::Dot),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Let {
        mutable: bool,
        name: String,
        expected_type: Type,
        expression: Expr,
    },
    Assignment {
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
    Schedule(Schedule),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Component {
    pub name: String,
    pub fields: Vec<Field>,
}

impl Component {
    /// Cherche dans `self.fields` un champ nommé `field_name`.
    /// Si trouvé, renvoie `Some(&Type)`, sinon `None`.
    pub fn get_field_type(&self, field_name: &str) -> Option<&Type> {
        self.fields.iter().find_map(|field| match field {
            Field::Named(name, ty) if name == field_name => Some(ty),
            _ => None,
        })
    }

    /// Si, au lieu du Type, vous voulez récupérer tout le Field (par exemple pour distinguer Named/Unnamed)
    pub fn get_field(&self, field_name: &str) -> Option<&Field> {
        self.fields
            .iter()
            .find(|field| matches!(field, Field::Named(name, _) if name == field_name))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Resource {
    pub name: String,
    pub fields: Vec<Field>,
}

/// in Component
#[derive(Debug, Clone, PartialEq)]
pub enum Field {
    Named(String, Type),
    Unnamed(Type),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: String,
    pub mutable: bool,
    pub param_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct System {
    pub name: String,
    pub params: Vec<Param>,
    pub resources: Vec<Param>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Schedule {
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
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
