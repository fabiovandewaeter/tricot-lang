use std::collections::HashMap;

use crate::parser::ast::*;

pub struct Interpreter {
    env: std::collections::HashMap<String, i64>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: HashMap::new(),
        }
    }

    pub fn run(&mut self, prog: Program) {
        for stmt in prog.statements {
            match stmt {
                Stmt::Let { name, value } => {
                    let val = self.eval_expr(value);
                    self.env.insert(name, val);
                }
                Stmt::Expr(expr) => {
                    let val = self.eval_expr(expr);
                    println!("=> {}", val);
                }
            }
        }
    }

    fn eval_expr(&self, expr: Expr) -> i64 {
        match expr {
            Expr::Number(n) => n,
            Expr::Identifier(id) => *self
                .env
                .get(&id)
                .expect(&format!("Undefined Identifier : {}", id)),
            Expr::BinaryOp { left, op, right } => {
                let l = self.eval_expr(*left);
                let r = self.eval_expr(*right);
                match op {
                    BinaryOp::Plus => l + r,
                    BinaryOp::Minus => l - r,
                    BinaryOp::Star => l * r,
                    BinaryOp::Slash => l / r,
                }
            }
            Expr::Call { callee, args } => todo!(),
        }
    }
}
