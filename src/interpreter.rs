use std::collections::HashMap;

use crate::{parser::ast::*, values::Value};

pub struct Interpreter {
    global_env: HashMap<String, Value>,
    stack: Vec<HashMap<String, Value>>,
    functions: HashMap<String, Function>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            global_env: HashMap::new(),
            stack: vec![HashMap::new()],
            functions: HashMap::new(),
        }
    }

    pub fn run(&mut self, prog: Program) {
        // Register functions first
        for stmt in &prog.statements {
            if let Stmt::Function(func) = stmt {
                self.functions.insert(func.name.clone(), func.clone());
            }
        }

        // Execute statements
        for stmt in prog.statements {
            if !matches!(stmt, Stmt::Function(_)) {
                self.run_stmt(stmt);
            }
        }
    }

    fn run_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Let {
                mutable: _,
                name,
                expression,
                ..
            } => {
                let val = self.eval_expr(expression);
                self.set_variable(&name, val);
            }

            Stmt::Assignment { target, expression } => {
                let rhs = self.eval_expr(expression);
                self.assign_target(target, rhs);
            }

            Stmt::CompoundAssignment {
                target,
                op,
                expression,
            } => {
                let rhs = self.eval_expr(expression);
                self.compound_assign(target, op, rhs);
            }

            Stmt::Expr(expr) => {
                let val = self.eval_expr(expr);
                if self.stack.len() == 1 {
                    // Top-level only
                    println!("=> {:?}", val);
                }
            }

            _ => {} // Ignore other statement types
        }
    }

    fn eval_expr(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Number(n) => Value::Int(n),
            Expr::Identifier(name) => self.get_variable(&name),
            Expr::StringLiteral(s) => Value::String(s),

            Expr::UnaryOp { op, expr } => self.eval_unary_op(op, *expr),
            Expr::BinaryOp { left, op, right } => self.eval_binary_op(*left, op, *right),

            Expr::Call { callee, args } => {
                if let Expr::Identifier(name) = *callee {
                    self.call_function(&name, args)
                } else {
                    panic!("Cannot call non-identifier expression");
                }
            }
        }
    }

    // Helper functions
    pub fn get_variable(&self, name: &str) -> Value {
        // Search stack frames (top to bottom)
        for env in self.stack.iter().rev() {
            if let Some(val) = env.get(name) {
                return val.clone();
            }
        }

        // Search global environment
        if let Some(val) = self.global_env.get(name) {
            return val.clone();
        }

        panic!("Undefined identifier: {}", name)
    }

    fn set_variable(&mut self, name: &str, value: Value) {
        // If in function, set in current stack frame
        if self.stack.len() > 1 {
            if let Some(env) = self.stack.last_mut() {
                env.insert(name.to_string(), value);
                return;
            }
        }

        // Otherwise set in global
        self.global_env.insert(name.to_string(), value);
    }

    fn get_variable_skip(&self, name: &str, skip: usize) -> Value {
        let stack_len = self.stack.len();
        if skip >= stack_len {
            // After skipping, only global remains
            self.global_env
                .get(name)
                .cloned()
                .unwrap_or_else(|| panic!("Undefined variable: {}", name))
        } else {
            // Skip the top 'skip' frames
            for env in self.stack.iter().rev().skip(skip) {
                if let Some(val) = env.get(name) {
                    return val.clone();
                }
            }
            self.global_env
                .get(name)
                .cloned()
                .unwrap_or_else(|| panic!("Undefined variable: {}", name))
        }
    }

    fn update_variable_skip(&mut self, name: &str, value: Value, skip: usize) {
        let stack_len = self.stack.len();
        if skip >= stack_len {
            // Update global variable
            if self.global_env.contains_key(name) {
                self.global_env.insert(name.to_string(), value);
                return;
            }
        } else {
            // Skip the top 'skip' frames
            for env in self.stack.iter_mut().rev().skip(skip) {
                if env.contains_key(name) {
                    env.insert(name.to_string(), value.clone());
                    return;
                }
            }
            // Update global
            if self.global_env.contains_key(name) {
                self.global_env.insert(name.to_string(), value);
                return;
            }
        }
        panic!("Assignment to undefined variable: {}", name);
    }

    fn assign_target(&mut self, target: Expr, value: Value) {
        match target {
            Expr::Identifier(name) => self.set_variable(&name, value),

            Expr::UnaryOp {
                op: UnaryOp::Deref,
                expr,
            } => {
                let ref_value = self.eval_expr(*expr);
                if let Value::Reference {
                    name,
                    depth_at_creation,
                    mutable,
                } = ref_value
                {
                    if !mutable {
                        panic!("Cannot assign through immutable reference");
                    }
                    let skip = self.stack.len() - depth_at_creation;
                    self.update_variable_skip(&name, value, skip);
                } else {
                    panic!("Cannot dereference non-reference value");
                }
            }

            _ => panic!("Invalid assignment target"),
        }
    }

    fn compound_assign(&mut self, target: Expr, op: BinaryOp, rhs: Value) {
        let (current_value, name, depth_at_creation, mutable) = match &target {
            Expr::Identifier(name) => {
                let val = self.get_variable(name);
                (val, name.clone(), 0, true)
            }

            Expr::UnaryOp {
                op: UnaryOp::Deref,
                expr,
            } => {
                let ref_value = self.eval_expr(*expr.clone());
                if let Value::Reference {
                    name,
                    depth_at_creation,
                    mutable,
                } = ref_value
                {
                    if !mutable {
                        panic!("Cannot assign through immutable reference");
                    }
                    let skip = self.stack.len() - depth_at_creation;
                    let val = self.get_variable_skip(&name, skip);
                    (val, name, depth_at_creation, mutable)
                } else {
                    panic!("Cannot dereference non-reference value");
                }
            }

            _ => panic!("Invalid compound assignment target"),
        };

        let new_value = self.apply_binary_op(current_value, op, rhs);

        match target {
            Expr::Identifier(name) => {
                self.set_variable(&name, new_value);
            }

            Expr::UnaryOp {
                op: UnaryOp::Deref,
                expr: _,
            } => {
                let skip = self.stack.len() - depth_at_creation;
                self.update_variable_skip(&name, new_value, skip);
            }

            _ => unreachable!(),
        }
    }

    fn eval_unary_op(&mut self, op: UnaryOp, expr: Expr) -> Value {
        match op {
            UnaryOp::AddrOf(mutable) => {
                if let Expr::Identifier(name) = expr {
                    let depth_at_creation = self.stack.len();
                    Value::Reference {
                        name,
                        depth_at_creation,
                        mutable,
                    }
                } else {
                    panic!("Cannot take address of non-identifier");
                }
            }

            UnaryOp::Deref => {
                let value = self.eval_expr(expr);
                if let Value::Reference {
                    name,
                    depth_at_creation,
                    ..
                } = value
                {
                    let skip = self.stack.len() - depth_at_creation;
                    self.get_variable_skip(&name, skip)
                } else {
                    panic!("Cannot dereference non-reference value");
                }
            }
        }
    }

    fn eval_binary_op(&mut self, left: Expr, op: BinaryOp, right: Expr) -> Value {
        let lval = self.eval_expr(left);
        let rval = self.eval_expr(right);
        self.apply_binary_op(lval, op, rval)
    }

    fn apply_binary_op(&self, left: Value, op: BinaryOp, right: Value) -> Value {
        match (left, right) {
            (Value::Int(l), Value::Int(r)) => {
                let result = match op {
                    BinaryOp::Plus => l + r,
                    BinaryOp::Minus => l - r,
                    BinaryOp::Star => l * r,
                    BinaryOp::Slash => l / r,
                    _ => panic!("Unsupported operation for integers"),
                };
                Value::Int(result)
            }

            (Value::String(l), Value::String(r)) if op == BinaryOp::Plus => {
                Value::String(format!("{}{}", l, r))
            }

            _ => panic!("Type mismatch or unsupported operation"),
        }
    }

    fn call_function(&mut self, name: &str, args: Vec<Expr>) -> Value {
        match name {
            "print" => self.call_print(args),
            _ => self.call_user_function(name, args),
        }
    }

    fn call_print(&mut self, args: Vec<Expr>) -> Value {
        for arg in args {
            let val = self.eval_expr(arg);
            println!("{:?}", val);
        }
        Value::Null
    }

    fn call_user_function(&mut self, name: &str, args: Vec<Expr>) -> Value {
        let func = self
            .functions
            .get(name)
            .unwrap_or_else(|| panic!("Undefined function: {}", name))
            .clone();

        // Evaluate arguments
        let evaluated_args: Vec<Value> = args.into_iter().map(|arg| self.eval_expr(arg)).collect();

        // Create new stack frame
        let mut frame = HashMap::new();
        for (
            Param {
                name,
                mutable,
                param_type,
            },
            value,
        ) in func.params.iter().zip(evaluated_args.iter())
        {
            frame.insert(name.clone(), value.clone());
        }

        // Execute function body
        self.stack.push(frame);
        let mut result = Value::Null;

        for stmt in func.body {
            if let Stmt::Expr(expr) = stmt {
                result = self.eval_expr(expr);
            } else {
                self.run_stmt(stmt);
            }
        }

        self.stack.pop();
        result
    }
}
