use std::collections::HashMap;

use crate::{parser::ast::*, values::Value};

pub struct Interpreter {
    env: std::collections::HashMap<String, Value>,
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
                    println!("=> {:?}", val);
                }
            }
        }
    }

    fn eval_expr(&self, expr: Expr) -> Value {
        match expr {
            Expr::Number(n) => Value::Int(n),
            Expr::Identifier(id) => self
                .env
                .get(&id)
                .cloned()
                .expect(&format!("Undefined Identifier: {}", id)),
            Expr::StringLiteral(string_literal) => Value::String(string_literal),
            Expr::BinaryOp { left, op, right } => {
                let l = self.eval_expr(*left);
                let r = self.eval_expr(*right);
                match (l, r) {
                    (Value::Int(lv), Value::Int(rv)) => {
                        let result = match op {
                            BinaryOp::Plus => lv + rv,
                            BinaryOp::Minus => lv - rv,
                            BinaryOp::Star => lv * rv,
                            BinaryOp::Slash => lv / rv,
                        };
                        Value::Int(result)
                    }
                    _ => panic!("Type error in binary operation"),
                }
            }
            Expr::Call { callee, args } => {
                if let Expr::Identifier(name) = *callee {
                    let vals: Vec<Value> =
                        args.into_iter().map(|arg| self.eval_expr(arg)).collect();
                    if name == "print" {
                        for val in &vals {
                            println!("{:?}", val);
                        }
                        Value::Null
                    } else {
                        panic!("Unknown function `{}`", name);
                    }
                } else {
                    panic!("Cannot call non-identifier expression");
                }
            }
        }
    }
}
