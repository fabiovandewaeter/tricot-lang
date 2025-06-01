use super::types::Type;
use crate::parser::ast::{BinaryOp, Expr, Function, Program, Stmt, UnaryOp};
use std::collections::HashMap;

struct TypeContext {
    functions: HashMap<String, Function>,
    variables: HashMap<String, (Type, bool)>,
}

impl TypeContext {
    fn new() -> Self {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    fn infer_type(&self, expression: &Expr) -> Type {
        match expression {
            Expr::Number(_) => Type::Int,
            Expr::StringLiteral(_) => Type::String,

            Expr::Identifier(name) => self
                .variables
                .get(name)
                .map(|(ty, _)| ty.clone())
                .unwrap_or_else(|| panic!("Undefined variable: {}", name)),

            Expr::UnaryOp { op, expr } => self.infer_unary_type(op, expr),
            Expr::BinaryOp { left, op, right } => self.infer_binary_type(left, op, right),
            Expr::Call { callee, args } => self.infer_call_type(callee, args),
        }
    }

    fn infer_unary_type(&self, op: &UnaryOp, expr: &Expr) -> Type {
        let inner_type = self.infer_type(expr);
        match op {
            UnaryOp::Deref => match inner_type {
                Type::Reference { inner, mutable: _ } => *inner,
                _ => panic!("Cannot dereference non-reference type: {:?}", inner_type),
            },
            UnaryOp::AddrOf(mutable) => Type::Reference {
                inner: Box::new(inner_type),
                mutable: *mutable,
            },
        }
    }

    fn infer_binary_type(&self, left: &Expr, op: &BinaryOp, right: &Expr) -> Type {
        let l_type = self.infer_type(left);
        let r_type = self.infer_type(right);

        if l_type != r_type {
            panic!(
                "Type mismatch in binary operation: {:?} {:?} {:?}",
                l_type, op, r_type
            );
        }

        l_type
    }

    fn infer_call_type(&self, callee: &Expr, args: &[Expr]) -> Type {
        let function_name = match callee {
            Expr::Identifier(id) => id,
            _ => panic!("Callee must be an identifier"),
        };

        let function = self
            .functions
            .get(function_name)
            .unwrap_or_else(|| panic!("Function '{}' not found", function_name));

        // Vérifier les arguments
        for (i, (arg, param)) in args.iter().zip(function.params.iter()).enumerate() {
            let arg_type = self.infer_type(arg);
            if arg_type != param.1 {
                panic!(
                    "Argument {} mismatch in call to '{}': expected {:?}, found {:?}",
                    i, function_name, param.1, arg_type
                );
            }
        }

        function.return_type.clone()
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

    fn resolve_lvalue_type(&self, expr: &Expr) -> Type {
        match expr {
            Expr::Identifier(name) => self
                .variables
                .get(name)
                .map(|(ty, _)| ty.clone())
                .unwrap_or_else(|| panic!("Undefined variable: {}", name)),

            Expr::UnaryOp {
                op: UnaryOp::Deref,
                expr,
            } => {
                let ty = self.infer_type(expr);
                if let Type::Reference { inner, .. } = ty {
                    *inner.clone()
                } else {
                    panic!("Cannot dereference non-reference type")
                }
            }

            _ => panic!("Invalid l-value expression: {:?}", expr),
        }
    }
}

pub struct TypeChecker;

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    pub fn check(&mut self, prog: &mut Program) {
        let mut global_context = TypeContext::new();

        // Enregistrer les fonctions
        for stmt in &prog.statements {
            if let Stmt::Function(func) = stmt {
                global_context
                    .functions
                    .insert(func.name.clone(), func.clone());
            }
        }

        // Vérifier les déclarations
        for stmt in &prog.statements {
            match stmt {
                Stmt::Function(func) => self.check_function(&global_context, func),
                _ => self.check_stmt(&mut global_context, stmt),
            }
        }
    }

    fn check_function(&self, global_context: &TypeContext, function: &Function) {
        let mut local_context = TypeContext {
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        // Ajouter les paramètres
        for (name, ty) in &function.params {
            local_context
                .variables
                .insert(name.clone(), (ty.clone(), false));
        }

        // Vérifier le corps
        for stmt in &function.body {
            self.check_stmt(&mut local_context, stmt);
        }

        // Vérifier le type de retour
        let last_expr = function.body.last();
        if function.return_type != Type::Null {
            if let Some(Stmt::Expr(expr)) = last_expr {
                let actual_type = local_context.infer_type(expr);
                if actual_type != function.return_type {
                    panic!(
                        "Return type mismatch in {}: expected {:?}, found {:?}",
                        function.name, function.return_type, actual_type
                    );
                }
            } else {
                panic!("Function {} should return a value", function.name);
            }
        }
    }

    fn check_stmt(&self, context: &mut TypeContext, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                mutable,
                name,
                expression,
                expected_type,
            } => self.check_variable_declaration(context, mutable, name, expression, expected_type),

            Stmt::Assignment { target, expression } => {
                self.check_assignment(context, target, expression)
            }

            Stmt::CompoundAssignment {
                target,
                op,
                expression,
            } => self.check_compound_assignment(context, target, op, expression),

            Stmt::Expr(expr) => self.check_expr(context, expr),
            Stmt::Function(_) => (), // Déjà géré

            _ => panic!("Unsupported statement: {:?}", stmt),
        }
    }

    fn check_variable_declaration(
        &self,
        context: &mut TypeContext,
        mutable: &bool,
        name: &str,
        expression: &Expr,
        expected_type: &Type,
    ) {
        self.check_expr(context, expression);
        let inferred_type = context.infer_type(expression);

        if *expected_type != Type::UNDEFINED && *expected_type != inferred_type {
            panic!(
                "Type mismatch for '{}': expected '{:?}' but found '{:?}'",
                name, expected_type, inferred_type
            );
        }

        context
            .variables
            .insert(name.to_string(), (inferred_type, *mutable));
    }

    fn check_assignment(&self, context: &mut TypeContext, target: &Expr, expression: &Expr) {
        self.check_expr(context, target);
        self.check_expr(context, expression);

        if !context.is_expr_mutable(target) {
            panic!("Cannot assign to immutable expression: {:?}", target);
        }

        let target_type = context.resolve_lvalue_type(target);
        let expr_type = context.infer_type(expression);

        if target_type != expr_type {
            panic!(
                "Assignment type mismatch: expected {:?}, found {:?}",
                target_type, expr_type
            );
        }
    }

    fn check_compound_assignment(
        &self,
        context: &mut TypeContext,
        target: &Expr,
        op: &BinaryOp,
        expression: &Expr,
    ) {
        self.check_expr(context, target);
        self.check_expr(context, expression);

        if !context.is_expr_mutable(target) {
            panic!("Cannot assign to immutable expression in compound assignment");
        }

        let target_type = context.resolve_lvalue_type(target);
        let expr_type = context.infer_type(expression);

        if target_type != expr_type {
            panic!(
                "Type mismatch in compound assignment: expected {:?}, found {:?}",
                target_type, expr_type
            );
        }

        // Vérifier que l'opération est valide pour le type
        if target_type != Type::Int {
            panic!("Compound operations only supported for integers");
        }
    }

    fn check_expr(&self, context: &mut TypeContext, expr: &Expr) {
        match expr {
            Expr::UnaryOp {
                op: UnaryOp::AddrOf(true),
                expr,
            } => {
                if !context.is_expr_mutable(expr) {
                    panic!("Cannot borrow immutable expression as mutable: {:?}", expr);
                }
                self.check_expr(context, expr);
            }

            Expr::UnaryOp { expr, .. } => self.check_expr(context, expr),
            Expr::BinaryOp { left, right, .. } => {
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
