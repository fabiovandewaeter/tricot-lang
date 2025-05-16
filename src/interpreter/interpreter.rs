use std::collections::HashMap;

use crate::{parser::ast::*, values::Value};

pub struct Interpreter {
    env: std::collections::HashMap<String, Value>,
    functions: HashMap<String, Function>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn run(&mut self, prog: Program) {
        for stmt in prog.statements {
            match stmt {
                Stmt::Function(func) => {
                    self.functions.insert(func.name.clone(), func);
                }
                _ => self.run_stmt(stmt),
            }
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> Value {
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
                    if name == "print" {
                        let vals: Vec<Value> =
                            args.into_iter().map(|arg| self.eval_expr(arg)).collect();
                        for val in &vals {
                            println!("{:?}", val);
                        }
                        Value::Null
                    } else {
                        // 1. Cloner les données nécessaires avant de modifier self
                        let (params, body) = {
                            let func = self.functions.get(&name).expect("Fonction inconnue");
                            (func.params.clone(), func.body.clone())
                        };
                        // 2. Évaluer les arguments APRÈS avoir libéré l'emprunt de func
                        let args_eval: Vec<Value> =
                            args.into_iter().map(|a| self.eval_expr(a)).collect();
                        // 3. Préparer le nouvel environnement
                        let mut new_env = HashMap::new();
                        for (param, val) in params.iter().zip(args_eval) {
                            new_env.insert(param.clone(), val);
                        }

                        // 4. Sauvegarder l'ancien environnement
                        let old_env = std::mem::replace(&mut self.env, new_env);

                        // 5. Exécuter le corps avec le nouvel environnement
                        for stmt in body {
                            self.run_stmt(stmt);
                        }

                        // 6. Restaurer l'environnement original
                        self.env = old_env;

                        Value::Null
                    }
                } else {
                    panic!("Cannot call non-identifier expression");
                }
            }
        }
    }

    fn run_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Let { name, value } => {
                let val = self.eval_expr(value);
                self.env.insert(name, val);
            }
            Stmt::Expr(expr) => {
                let val = self.eval_expr(expr);
                println!("=> {:?}", val);
            }
            _ => panic!("Instruction non supportée dans une fonction"),
        }
    }
}
