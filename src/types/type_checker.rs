use super::types::Type;
use crate::parser::ast::{
    BinaryOp, Component, Expr, Field, Function, Param, Program, Resource, Schedule, Setup, Stmt,
    System, UnaryOp,
};
use std::collections::HashMap;

#[derive(Debug)]
struct TypeContext {
    components: HashMap<String, Component>,
    resources: HashMap<String, Resource>,
    systems: HashMap<String, System>,
    schedule: Option<Schedule>,
    setup: Option<Setup>,
    functions: HashMap<String, Function>,
    variables: HashMap<String, (Type, bool)>,
}

impl TypeContext {
    fn new() -> Self {
        Self {
            components: HashMap::new(),
            resources: HashMap::new(),
            systems: HashMap::new(),
            schedule: None,
            setup: None,
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
            Expr::Spawn(component_inits) => self.check_entity_spawn(component_inits),
            Expr::CallSystem { callee, once } => todo!(),
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
        match op {
            BinaryOp::Dot => {
                if let Expr::Identifier(left_name) = left {
                    if let Expr::Identifier(right_name) = right {
                        if let Some((left_type, _)) = self.variables.get(left_name) {
                            match left_type {
                                Type::Component(comp_name) => {
                                    if let Some(component) = self.components.get(comp_name) {
                                        component.get_field_type(right_name).unwrap().clone()
                                    } else {
                                        panic!("Can't find Component: {:?}", comp_name)
                                    }
                                }
                                Type::Resource(res_name) => {
                                    if let Some(resource) = self.resources.get(res_name) {
                                        resource.get_field_type(right_name).unwrap().clone()
                                    } else {
                                        panic!("Can't find Resource: {:?}", res_name)
                                    }
                                }
                                _ => panic!("Cannot use dot operator on type {:?}", left_type),
                            }
                        } else {
                            panic!("Can't find variable: {:?}", left_name)
                        }
                    } else {
                        panic!("Right side of dot must be an identifier")
                    }
                } else {
                    panic!("Left side of dot must be an identifier")
                }
            }

            _ => {
                let l_type = self.infer_type(left);
                let r_type = self.infer_type(right);

                // Special case for resource fields
                if let (Type::Resource(_), Type::Int) = (&l_type, &r_type) {
                    return l_type.clone();
                }

                if l_type != r_type {
                    panic!(
                        "Type mismatch in binary operation: {:?} {:?} {:?}",
                        l_type, op, r_type
                    );
                }

                l_type
            }
        }
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

        for (i, (arg, param)) in args.iter().zip(function.params.iter()).enumerate() {
            let arg_type = self.infer_type(arg);
            if arg_type != param.param_type {
                panic!(
                    "Argument {} mismatch in call to '{}': expected {:?}, found {:?}",
                    i, function_name, param.param_type, arg_type
                );
            }
        }

        function.return_type.clone()
    }

    fn check_entity_spawn(&self, component_inits: &Vec<Expr>) -> Type {
        for init_expr in component_inits {
            match init_expr {
                Expr::Call { callee, args } => {
                    let comp_name = match &**callee {
                        Expr::Identifier(id) => id,
                        other => panic!(
                            "Spawn attends un appel de constructeur (nom du composant). Mais trouvé : {:?}",
                            other
                        ),
                    };

                    let component = self.components.get(comp_name).unwrap_or_else(|| {
                        panic!("Composant inconnu dans spawn: {}", comp_name);
                    });

                    if args.len() != component.fields.len() {
                        panic!(
                            "Le composant '{}' attend {} champ(s), mais le spawn en fournit {}",
                            comp_name,
                            component.fields.len(),
                            args.len()
                        );
                    }

                    for (i, (field, arg_expr)) in
                        component.fields.iter().zip(args.iter()).enumerate()
                    {
                        let expected_ty = match field {
                            Field::Named(_, ty) => ty.clone(),
                            Field::Unnamed(ty) => ty.clone(),
                        };
                        let actual_ty = self.infer_type(arg_expr);
                        if actual_ty != expected_ty {
                            panic!(
                                "Type mismatch pour le champ #{} du composant '{}' : \
                                        attendu {:?}, trouvé {:?}",
                                i, comp_name, expected_ty, actual_ty
                            );
                        }
                    }
                }

                other => {
                    panic!(
                        "Dans spawn, chaque élément doit être un appel de constructeur de composant, \
                                trouvé : {:?}",
                        other
                    );
                }
            }
        }
        Type::Int
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

            Expr::BinaryOp { left, op, right: _ } => match op {
                BinaryOp::Dot => {
                    if let Expr::Identifier(name) = &**left {
                        self.variables
                            .get(name)
                            .map(|(_, mutable)| *mutable)
                            .unwrap_or(false)
                    } else {
                        false
                    }
                }
                _ => false,
            },

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

            Expr::BinaryOp { left, op, right } => match op {
                BinaryOp::Dot => {
                    if let Expr::Identifier(left_name) = &**left {
                        if let Expr::Identifier(right_name) = &**right {
                            if let Some((ty, _)) = self.variables.get(left_name) {
                                match ty {
                                    Type::Component(comp_name) => {
                                        if let Some(component) = self.components.get(comp_name) {
                                            component.get_field_type(right_name).unwrap().clone()
                                        } else {
                                            panic!("Can't find Component: {:?}", comp_name)
                                        }
                                    }
                                    Type::Resource(res_name) => {
                                        if let Some(resource) = self.resources.get(res_name) {
                                            resource.get_field_type(right_name).unwrap().clone()
                                        } else {
                                            panic!("Can't find Resource: {:?}", res_name)
                                        }
                                    }
                                    _ => panic!("Cannot use dot operator on type {:?}", ty),
                                }
                            } else {
                                panic!("Can't find variable: {:?}", left_name)
                            }
                        } else {
                            panic!("Right side of dot must be an identifier")
                        }
                    } else {
                        panic!("Left side of dot must be an identifier")
                    }
                }
                _ => panic!("Can't resolve type of lvalue for that BinaryOp"),
            },

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

        for stmt in &prog.statements {
            match stmt {
                Stmt::Component(component) => {
                    global_context
                        .components
                        .insert(component.name.clone(), component.clone());
                }

                Stmt::Resource(resource) => {
                    global_context
                        .resources
                        .insert(resource.name.clone(), resource.clone());
                }

                Stmt::System(system) => {
                    global_context
                        .systems
                        .insert(system.name.clone(), system.clone());

                    self.check_system(&global_context, system)
                }

                Stmt::Schedule(schedule) => {
                    global_context.schedule = Some(schedule.clone());
                    self.check_schedule(&global_context, schedule)
                }

                Stmt::Setup(setup) => {
                    global_context.setup = Some(setup.clone());
                    self.check_setup(&global_context, setup)
                }

                Stmt::Function(func) => {
                    global_context
                        .functions
                        .insert(func.name.clone(), func.clone());
                    self.check_function(&global_context, func)
                }

                _ => self.check_stmt(&mut global_context, stmt),
            }
        }
    }

    fn check_system(&self, global_context: &TypeContext, system: &System) {
        let mut local_context = TypeContext {
            components: global_context.components.clone(),
            resources: global_context.resources.clone(),
            systems: global_context.systems.clone(),
            schedule: global_context.schedule.clone(),
            setup: global_context.setup.clone(),
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        for Param {
            name,
            param_type,
            mutable,
        } in &system.params
        {
            local_context
                .variables
                .insert(name.clone(), (param_type.clone(), *mutable));
        }

        for Param {
            name,
            param_type,
            mutable,
        } in &system.resources
        {
            local_context
                .variables
                .insert(name.clone(), (param_type.clone(), *mutable));
        }

        for stmt in &system.body {
            self.check_stmt(&mut local_context, stmt);
        }
    }

    fn check_schedule(&self, global_context: &TypeContext, schedule: &Schedule) {
        let local_context = TypeContext {
            components: global_context.components.clone(),
            resources: global_context.resources.clone(),
            systems: global_context.systems.clone(),
            schedule: global_context.schedule.clone(),
            setup: global_context.setup.clone(),
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        for stmt in &schedule.body {
            match stmt {
                Stmt::Expr(expr) => match expr {
                    Expr::CallSystem { callee, once: _ } => {
                        if let Expr::Identifier(callee_name) = &**callee {
                            local_context.systems.get(callee_name).unwrap_or_else(|| {
                                panic!("Calls a system that does not exist: {:?}", callee_name);
                            });
                        } else {
                            panic!(
                                "Le callee de CallSystem doit être un Identifier, trouvé: {:?}",
                                callee
                            );
                        }
                    }

                    other => panic!(
                        "Identifier in a Schedule should has the name of a System: {:?}",
                        other
                    ),
                },

                _ => panic!("Unsupported statement in Schedule: {:?}", stmt),
            }
        }
    }

    fn check_setup(&self, global_context: &TypeContext, setup: &Setup) {
        let mut local_context = TypeContext {
            components: global_context.components.clone(),
            resources: global_context.resources.clone(),
            systems: global_context.systems.clone(),
            schedule: global_context.schedule.clone(),
            setup: global_context.setup.clone(),
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        for stmt in &setup.body {
            if let Stmt::Init { name, fields } = stmt {
                let resource = global_context.resources.get(name).unwrap_or_else(|| {
                    panic!("Unknown resource in setup: {}", name);
                });

                if fields.len() != resource.fields.len() {
                    panic!(
                        "Resource {} expects {} fields, found {}",
                        name,
                        resource.fields.len(),
                        fields.len()
                    );
                }

                for (i, ((field_name_opt, expr), resource_field)) in
                    fields.iter().zip(resource.fields.iter()).enumerate()
                {
                    let expected_type = match resource_field {
                        Field::Named(_, ty) => ty,
                        Field::Unnamed(ty) => ty,
                    };

                    let actual_type = local_context.infer_type(expr);
                    if actual_type != *expected_type {
                        panic!(
                            "Type mismatch for field {} in resource {}: expected {:?}, found {:?}",
                            i, name, expected_type, actual_type
                        );
                    }

                    if let Some(field_name) = field_name_opt {
                        if let Field::Named(res_field_name, _) = resource_field {
                            if field_name != res_field_name {
                                panic!(
                                    "Field name mismatch: expected '{}', found '{}'",
                                    res_field_name, field_name
                                );
                            }
                        } else {
                            panic!(
                                "Named field initialization for unnamed field in resource {}",
                                name
                            );
                        }
                    } else {
                        if let Field::Named(_, _) = resource_field {
                            panic!(
                                "Unnamed field initialization for named field in resource {}",
                                name
                            );
                        }
                    }
                }
            } else {
                panic!("Setup block should only contain Init statements");
            }
        }
    }

    fn check_function(&self, global_context: &TypeContext, function: &Function) {
        let mut local_context = TypeContext {
            components: global_context.components.clone(),
            resources: global_context.resources.clone(),
            systems: global_context.systems.clone(),
            schedule: global_context.schedule.clone(),
            setup: global_context.setup.clone(),
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        for Param {
            name,
            param_type,
            mutable,
        } in &function.params
        {
            local_context
                .variables
                .insert(name.clone(), (param_type.clone(), *mutable));
        }

        for stmt in &function.body {
            self.check_stmt(&mut local_context, stmt);
        }

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

            Stmt::System(_) => (),

            Stmt::Function(_) => (),

            Stmt::Init { name, fields } => self.check_resource_init(context, name, fields),

            _ => panic!("Unsupported statement: {:?}", stmt),
        }
    }

    fn check_resource_init(
        &self,
        context: &mut TypeContext,
        name: &str,
        fields: &[(Option<String>, Expr)],
    ) {
        for (_, expr) in fields {
            self.check_expr(context, expr);
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
        _op: &BinaryOp,
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
