use std::collections::HashMap;

use crate::{parser::ast::*, types::types::Type, values::Value};

pub struct Interpreter {
    //env: HashMap<String, Value>,
    global_env: HashMap<String, Value>,
    stack: Vec<HashMap<String, Value>>,
    functions: HashMap<String, Function>,
    in_function: bool,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            //env: HashMap::new(),
            global_env: HashMap::new(),
            stack: vec![HashMap::new()],
            functions: HashMap::new(),
            in_function: false,
        }
    }

    pub fn run(&mut self, prog: Program) {
        // add all functions
        for stmt in &prog.statements {
            if let Stmt::Function(func) = stmt {
                self.functions.insert(func.name.clone(), func.clone());
            }
        }
        // execute
        for stmt in prog.statements {
            if let Stmt::Function(_) = stmt {
                continue;
            }
            self.run_stmt(stmt);
        }
    }

    /// find a variable in global environment or stack of environments
    /*fn lookup(&self, name: &str) -> Value {
        // Search call stack top-down
        for env in self.stack.iter().rev() {
            if let Some(v) = env.get(name) {
                return v.clone();
            }
        }
        // Check global env
        if let Some(v) = self.global_env.get(name) {
            return v.clone();
        }
        panic!("Undefined Identifier: {}", name)
    }*/
    fn lookup_skip(&self, name: &str, skip: usize) -> Value {
        // Parcourir les environnements du plus récent au plus ancien en sautant 'skip' cadres
        for (i, env) in self.stack.iter().enumerate().rev().skip(skip) {
            if let Some(v) = env.get(name) {
                return v.clone();
            }
        }
        // Chercher dans l'environnement global après avoir sauté les cadres
        if skip <= self.stack.len() {
            if let Some(v) = self.global_env.get(name) {
                return v.clone();
            }
        }
        panic!("Undefined Identifier: {}", name)
    }
    /// lookup standard (skip=0)
    pub fn lookup(&self, name: &str) -> Value {
        self.lookup_skip(name, 0)
    }

    fn update_variable(&mut self, name: &str, value: Value) {
        // Update in current stack frames
        for env in self.stack.iter_mut().rev() {
            if env.contains_key(name) {
                env.insert(name.to_string(), value);
                return;
            }
        }
        // Update in global env
        if self.global_env.contains_key(name) {
            self.global_env.insert(name.to_string(), value);
            return;
        }
        panic!("Assignment to undefined variable: {}", name);
    }

    /// Mise à jour de variable avec saut de cadres
    fn update_variable_skip(&mut self, name: &str, value: Value, skip: usize) {
        let mut skipped = 0;
        // Mettre à jour dans la pile
        for env in self.stack.iter_mut().rev() {
            if skipped < skip {
                skipped += 1;
                continue;
            }
            if env.contains_key(name) {
                env.insert(name.to_string(), value.clone());
                return;
            }
        }
        // Mettre à jour dans global
        if skipped < skip {
            skipped += 1;
        }
        if skipped >= skip {
            if self.global_env.contains_key(name) {
                self.global_env.insert(name.to_string(), value);
                return;
            }
        }
        panic!("Assignment to undefined variable: {}", name);
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
                if self.in_function {
                    // Insert into current stack frame
                    if let Some(env) = self.stack.last_mut() {
                        env.insert(name, val);
                    } else {
                        panic!("No stack frame available");
                    }
                } else {
                    // Global variable
                    self.global_env.insert(name, val);
                }
            }

            Stmt::Assignment { target, expression } => {
                let rhs = self.eval_expr(expression);
                match target {
                    // a = expr
                    Expr::Identifier(name) => {
                        //self.update_variable(&name, rhs);
                        self.update_variable_skip(&name, rhs, 0);
                    }
                    // *a = expr
                    Expr::UnaryOp {
                        op: UnaryOp::Deref,
                        expr,
                    } => {
                        if let Value::Reference {
                            name: target_name,
                            mutable,
                        } = self.eval_expr(*expr)
                        {
                            if !mutable {
                                panic!("Cannot assign through immutable reference");
                            }
                            //self.update_variable(&target_name, rhs);
                            self.update_variable_skip(&target_name, rhs, 1);
                        } else {
                            panic!("Cannot dereference non-reference value");
                        }
                    }
                    _ => panic!("Invalid assignment target"),
                }
            }

            Stmt::Expr(expr) => {
                let val = self.eval_expr(expr);
                // Only print results at top level
                if !self.in_function {
                    println!("=> {:?}", val);
                }
            }

            Stmt::Function(func) => {
                // Already handled in run()
                //self.functions.insert(func.name.clone(), func);
            }

            _ => panic!("Instruction non supportée dans une fonction"),
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Number(n) => Value::Int(n),

            Expr::Identifier(name) => self.lookup(&name),

            Expr::StringLiteral(string_literal) => Value::String(string_literal),

            Expr::UnaryOp { op, expr } => match op {
                UnaryOp::AddrOf(mutable) => {
                    if let Expr::Identifier(name) = *expr {
                        Value::Reference { name, mutable }
                    } else {
                        panic!("Cannot take address of non-identifier");
                    }
                }
                UnaryOp::Deref => {
                    let v = self.eval_expr(*expr);
                    if let Value::Reference { name, mutable: _ } = v {
                        //self.lookup(&name)
                        self.lookup_skip(&name, 1)
                    } else {
                        panic!("Cannot dereference non-reference value");
                    }
                }
            },

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
                        // print builtin
                        for arg in args {
                            let v = self.eval_expr(arg);
                            println!("{:?}", v);
                        }
                        Value::Null
                    } else {
                        let func = self.functions.get(&name).unwrap().clone();
                        // Evaluate arguments
                        let mut evaluated = Vec::new();
                        for arg in args {
                            evaluated.push(self.eval_expr(arg));
                        }

                        // Create new stack frame
                        let mut new_frame = HashMap::new();
                        for ((pname, _), argval) in func.params.iter().zip(evaluated.iter()) {
                            new_frame.insert(pname.clone(), argval.clone());
                        }

                        // Push frame and execute function
                        self.stack.push(new_frame);
                        let mut result = Value::Null;
                        for stmt in func.body {
                            match stmt {
                                Stmt::Expr(e) => {
                                    result = self.eval_expr(e);
                                }
                                _ => {
                                    self.run_stmt(stmt);
                                }
                            }
                        }
                        self.in_function = false;
                        self.stack.pop();
                        result
                    }
                } else {
                    panic!("Cannot call non-identifier expression");
                }
            }
        }
    }
}
