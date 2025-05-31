use std::collections::HashMap;

use crate::parser::ast::{Expr, Function, Program, Stmt, UnaryOp};

use super::types::Type;

struct TypeContext {
    functions: HashMap<String, Function>,
    /// name -> (Type, mutable)
    variables: HashMap<String, (Type, bool)>,
}

impl TypeContext {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    /// faire qu'on a soit le type de l'expression soit si c'est genre un call on regarde le type de retour de la fonction
    fn infer_type(&self, expression: &Expr) -> Type {
        match expression {
            Expr::Number(_) => Type::Int,

            Expr::StringLiteral(_) => Type::String,

            Expr::Identifier(name) => {
                if let Some(variable_info) = self.variables.get(name) {
                    variable_info.0.clone() // variable_info is a tuple of type (Type, bool)
                } else {
                    panic!("Undefined variable: {}", name)
                }
            }

            Expr::UnaryOp { op, expr } => {
                let inner_type = self.infer_type(expr);
                match op {
                    UnaryOp::Deref => {
                        if let Type::Reference { inner, mutable } = inner_type {
                            *inner
                        } else {
                            panic!("Canno dereference non-reference type: {:?}", inner_type)
                        }
                    }
                    UnaryOp::AddrOf(mutable) => Type::Reference {
                        inner: Box::new(inner_type),
                        mutable: *mutable,
                    },
                }
            }

            Expr::BinaryOp { left, op, right } => {
                let l_type = self.infer_type(&left);
                let r_type = self.infer_type(&right);
                if l_type == r_type {
                    l_type
                } else {
                    panic!("Error types BinaryOp : {:?} {:?}", l_type, r_type)
                }
            }

            Expr::Call { callee, args } => {
                if let Expr::Identifier(function_name) = &**callee {
                    self.functions
                        .get(function_name)
                        .expect(&format!("Function '{}' not found", function_name))
                        .return_type
                        .clone()
                } else {
                    panic!("{}", "Callee isn't an Identifier")
                }
            }
        }
    }

    fn is_expr_mutable(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name) => self
                .variables
                .get(name)
                .map(|(_, mutable)| *mutable)
                .unwrap_or(false),
            Expr::UnaryOp {
                op: UnaryOp::Deref,
                expr,
            } => {
                let ty = self.infer_type(expr);
                if let Type::Reference { mutable, .. } = ty {
                    mutable
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

pub struct TypeChecker {}

impl TypeChecker {
    pub fn new() -> Self {
        Self {}
    }

    pub fn check(&mut self, prog: &mut Program) {
        // add all functions to global context
        let mut global_context = TypeContext::new();
        for stmt in &prog.statements {
            if let Stmt::Function(func) = stmt {
                global_context
                    .functions
                    .insert(func.name.clone(), func.clone());
            }
        }

        for stmt in &prog.statements {
            if let Stmt::Function(func) = stmt {
                self.check_function(&global_context, func);
            }
        }
    }

    fn check_function(&self, global_context: &TypeContext, function: &Function) {
        // local context start as a copy of global_context
        let mut local_context = TypeContext {
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        // add parameters as variables in the local context
        for (param_name, param_type) in &function.params {
            local_context
                .variables
                .insert(param_name.clone(), (param_type.clone(), false));
        }

        for stmt in &function.body {
            self.check_stmt(&mut local_context, stmt);
        }
    }

    fn check_stmt(&self, context: &mut TypeContext, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                mutable,
                name,
                expected_type,
                expression,
            } => {
                let inferred_type = context.infer_type(expression);
                // check if the type of the expression and the expected type are the same
                if *expected_type != Type::UNDEFINED && *expected_type != inferred_type {
                    panic!(
                        "Type mismatch for '{}': expected '{:?}' but found '{:?}'",
                        name, expected_type, inferred_type
                    );
                }
                // else, add variable to context
                context
                    .variables
                    .insert(name.clone(), (inferred_type, mutable.clone()));
            }

            Stmt::Assignment { target, expression } => {
                self.check_expr(context, target);
                self.check_expr(context, expression);

                // get the name of the variable to assign the value to
                let var_name = match target {
                    // a = expr
                    Expr::Identifier(name) => name.clone(),
                    // *a = expr
                    Expr::UnaryOp {
                        op: UnaryOp::Deref,
                        expr,
                    } => {
                        if let Expr::Identifier(name) = &**expr {
                            name.clone()
                        } else {
                            panic!("Invalid l-value: cannot deref non-identifier");
                        }
                    }
                    _ => panic!("Invalid assigment target: {:?}", target),
                };
                // get informations on the variable
                let (declared_type, declared_mutable) = context.variables.get(&var_name).unwrap();

                // checks if variable is mutable
                if let Expr::UnaryOp {
                    op: UnaryOp::Deref,
                    expr,
                } = target
                {
                    // *a = expr
                    match declared_type {
                        Type::Reference { inner, mutable } if *mutable => {}
                        other => {
                            panic!("Cannot assign through immutable reference, got {:?}", other);
                        }
                    }
                } else {
                    // a = expr
                    if !*declared_mutable {
                        panic!("Cannot assign to immutable variable: {}", var_name);
                    }
                }

                // checks variable type
                let rhs_type = context.infer_type(expression);
                // if &mut T, extract T
                let expected = match declared_type {
                    Type::Reference { inner, mutable } => *inner.clone(),
                    other => other.clone(),
                };
                if rhs_type != expected {
                    println!("{:?}", stmt);
                    panic!(
                        "Type mismatch for '{}': expected '{:?}' but found '{:?}'",
                        var_name, declared_type, rhs_type
                    );
                }
            }

            Stmt::Expr(expr) => self.check_expr(context, expr),

            Stmt::Function(_) => {}

            _ => unreachable!("Error in TypeChecker::check_stmt()"),
        }
    }

    fn check_expr(&self, context: &mut TypeContext, expr: &Expr) {
        match expr {
            Expr::UnaryOp { op, expr } => {
                self.check_expr(context, expr);
                match op {
                    UnaryOp::AddrOf(true) => {
                        if !context.is_expr_mutable(expr) {
                            panic!("Cannot borrow immutable expression as mutable: {:?}", expr);
                        }
                    }
                    _ => {}
                }
            }

            Expr::BinaryOp { left, op: _, right } => {
                self.check_expr(context, left);
                self.check_expr(context, right);
            }

            Expr::Call { callee, args } => {
                self.check_expr(context, callee);
                for arg in args {
                    self.check_expr(context, arg);
                }
            }
            _ => (),
        }
    }
}
