use std::collections::HashMap;

use crate::parser::ast::{Expr, Function, Program, Stmt};

use super::types::Type;

struct TypeContext {
    functions: HashMap<String, Function>,
    variables: HashMap<String, Type>,
}

impl TypeContext {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    /// faire qu'on a soit le type de l'expression soit si c'est genre un call on regarde le type de retour de la fonction
    fn infer_type(&self, expression: &Expr) -> Result<Type, String> {
        match expression {
            Expr::Number(_) => Ok(Type::Int),
            Expr::StringLiteral(_) => Ok(Type::String),
            Expr::Identifier(name) => self
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Undefined variable : {}", name)),
            Expr::UnaryOp { op, expr } => self.infer_type(expr),
            Expr::BinaryOp { left, op, right } => {
                let l_type = self.infer_type(&left);
                let r_type = self.infer_type(&right);
                if l_type == r_type {
                    Ok(l_type?)
                } else {
                    Err(format!("Error types BinaryOp : {:?} {:?}", l_type, r_type))
                }
            }
            Expr::Call { callee, args } => {
                if let Expr::Identifier(function_name) = &**callee {
                    Ok(self
                        .functions
                        .get(function_name)
                        .expect(&format!("Function '{}' not found", function_name))
                        .return_type
                        .clone())
                } else {
                    Err("Callee isn't an Identifier".into())
                }
            }
        }
    }
}

pub struct TypeChecker {}

impl TypeChecker {
    pub fn new() -> Self {
        Self {}
    }

    pub fn check(&mut self, prog: &mut Program) -> Result<(), String> {
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
                self.check_function(&global_context, func)?;
            }
        }

        Ok(())
    }

    fn check_function(
        &self,
        global_context: &TypeContext,
        function: &Function,
    ) -> Result<(), String> {
        // local context start as a copy of global_context
        let mut local_context = TypeContext {
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        // add parameters as variables in the local context
        for (param_name, param_type) in &function.params {
            local_context
                .variables
                .insert(param_name.clone(), param_type.clone());
        }

        for stmt in &function.body {
            self.check_stmt(&mut local_context, stmt)?;
        }

        Ok(())
    }

    fn check_stmt(&self, context: &mut TypeContext, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Let {
                mutable,
                name,
                expected_type,
                expression,
            } => {
                let inferred_type = context.infer_type(expression)?;
                // check if the type of the expression and the expected type are the same
                if *expected_type != Type::UNDEFINED && *expected_type != inferred_type {
                    return Err(format!(
                        "Type mismatch for {}: expected {:?} but found {:?}",
                        name, expected_type, inferred_type
                    ));
                }
                // else, add variable to context
                context.variables.insert(name.clone(), inferred_type);
                Ok(())
            }
            Stmt::Expr(expr) => context.infer_type(expr).map(|_| ()),
            _ => unreachable!("Error in TypeChecker::check_stmt()"),
        }
    }
}
