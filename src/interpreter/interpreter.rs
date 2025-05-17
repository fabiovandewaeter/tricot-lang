use std::collections::HashMap;

use crate::{parser::ast::*, types::types::Type, values::Value};

pub struct Interpreter {
    env: std::collections::HashMap<String, Value>,
    functions: HashMap<String, Function>,
    in_function: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: HashMap::new(),
            functions: HashMap::new(),
            in_function: false,
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

            Expr::UnaryOp { op, expr } => todo!(),

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
                        // 1. Extraire les données de la fonction avant toute mutation
                        let (params, return_type, body) = {
                            let func = self
                                .functions
                                .get(&name)
                                .expect(&format!("Function '{}' not found", name));
                            (
                                func.params.clone(),
                                func.return_type.clone(),
                                func.body.clone(),
                            )
                        };

                        // 2. Évaluer les arguments APRÈS avoir libéré l'emprunt
                        let args_eval: Vec<Value> =
                            args.into_iter().map(|a| self.eval_expr(a)).collect();

                        // 3. Vérification des types des arguments
                        for (i, (_, param_type)) in params.iter().enumerate() {
                            match (&args_eval[i], param_type) {
                                (Value::Int(_), Type::Int) => (),
                                (Value::String(_), Type::String) => (),
                                _ => panic!("Type mismatch for argument {}", i),
                            }
                        }

                        // 4. Préparer le nouvel environnement
                        let new_env: HashMap<String, Value> = params
                            .iter()
                            .zip(args_eval.iter())
                            .map(|((name, _), val)| (name.clone(), val.clone()))
                            .collect();

                        // 5. Sauvegarder et remplacer l'environnement
                        let old_env = std::mem::replace(&mut self.env, new_env);

                        // 6. Exécuter le corps de la fonction
                        self.in_function = true;
                        let mut return_value = Value::Null;
                        for stmt in body {
                            match stmt {
                                Stmt::Let {
                                    mutable,
                                    name,
                                    expected_type,
                                    expression,
                                } => {
                                    // Exécuter les déclarations let
                                    let val = self.eval_expr(expression);
                                    self.env.insert(name, val);
                                }
                                Stmt::Expr(e) => {
                                    // Capturer la dernière valeur d'expression
                                    return_value = self.eval_expr(e);
                                }
                                _ => panic!("Instruction non supportée"),
                            }
                        }
                        self.in_function = false;

                        // 7. Vérifier le type de retour
                        match (&return_value, &return_type) {
                            (Value::Int(_), Type::Int) => (),
                            (Value::String(_), Type::String) => (),
                            (Value::Null, Type::Null) => (),
                            _ => panic!("Return type mismatch"),
                        }

                        if return_type == Type::Null {
                            return_value = Value::Null;
                        }

                        // 8. Restaurer l'environnement précédent
                        self.env = old_env;

                        return_value
                    }
                } else {
                    panic!("Cannot call non-identifier expression");
                }
            }
        }
    }

    fn run_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Let {
                mutable,
                name,
                expected_type,
                expression,
            } => {
                let val = self.eval_expr(expression);
                self.env.insert(name, val);
            }
            Stmt::Expr(expr) => {
                if !self.in_function {
                    let val = self.eval_expr(expr);
                    println!("=> {:?}", val);
                }
            }
            _ => panic!("Instruction non supportée dans une fonction"),
        }
    }
}
