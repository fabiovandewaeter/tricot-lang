use super::types::Type;
use crate::parser::ast::{
    BinaryOp, Component, Expr, Field, Function, Param, Program, Resource, Schedule, Stmt, System,
    UnaryOp,
};
use std::collections::HashMap;

#[derive(Debug)]
struct TypeContext {
    components: HashMap<String, Component>,
    resources: HashMap<String, Resource>,
    systems: HashMap<String, System>,
    schedule: Option<Schedule>,
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
                        if let Some((Type::Component(comp_name), _)) = self.variables.get(left_name)
                        {
                            if let Some(component) = self.components.get(comp_name) {
                                component.get_field_type(right_name).unwrap().clone()
                            } else {
                                println!("{:?}", self.components);
                                panic!("Can't find Component: {:?}", comp_name)
                            }
                        } else {
                            panic!("Can't find variable: {:?}", left_name)
                        }
                    } else {
                        panic!("Cannot use dot product on that type")
                    }
                } else {
                    panic!("Cannot use dot product on that type")
                }
            }

            _ => {
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

        // Vérifier les arguments
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
        // On s’assure que chaque élément est bien un appel de constructeur de composant
        for init_expr in component_inits {
            match init_expr {
                Expr::Call { callee, args } => {
                    // Le nom du composant doit exister dans self.components
                    let comp_name = match &**callee {
                        Expr::Identifier(id) => id,
                        other => panic!(
                            "Spawn attends un appel de constructeur (nom du composant). Mais trouvé : {:?}",
                            other
                        ),
                    };

                    // Vérifier que ce composant a bien été déclaré
                    let component = self.components.get(comp_name).unwrap_or_else(|| {
                        panic!("Composant inconnu dans spawn: {}", comp_name);
                    });

                    // Vérifier que le nombre et les types d’arguments correspondent aux champs du composant
                    if args.len() != component.fields.len() {
                        panic!(
                            "Le composant '{}' attend {} champ(s), mais le spawn en fournit {}",
                            comp_name,
                            component.fields.len(),
                            args.len()
                        );
                    }
                    // On peut parcourir component.fields et args en parallèle pour vérifier les types
                    for (i, (field, arg_expr)) in
                        component.fields.iter().zip(args.iter()).enumerate()
                    {
                        // Supposons que `field` soit toujours un `Field::Named(_, field_type)`
                        let expected_ty = match field {
                            Field::Named(_, ty) => ty.clone(),
                            Field::Unnamed(_) => {
                                panic!(
                                    "Dans votre implémentation, on n’utilise pas de champs anonymes"
                                )
                            }
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
        // Si tout est OK, on considère que le spawn renvoie un ID d’entité (Int)
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
                            if let Some((Type::Component(comp_name), _)) =
                                self.variables.get(left_name)
                            {
                                if let Some(component) = self.components.get(comp_name) {
                                    component.get_field_type(right_name).unwrap().clone()
                                } else {
                                    println!("{:?}", self.components);
                                    panic!("Can't find Component: {:?}", comp_name)
                                }
                            } else {
                                panic!("Can't find variable: {:?}", left_name)
                            }
                        } else {
                            panic!("Cannot use dot product on that type")
                        }
                    } else {
                        panic!("Cannot use dot product on that type")
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

        // add declarations and check Stmt
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
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        // add parameters to context
        for Param {
            name,
            param_type,
            mutable,
        } in &system.params
        {
            local_context
                .variables
                .insert(name.clone(), (param_type.clone(), mutable.clone()));
        }

        // add resources
        for Param {
            name,
            param_type,
            mutable,
        } in &system.resources
        {
            local_context
                .variables
                .insert(name.clone(), (param_type.clone(), mutable.clone()));
        }

        // checks body
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
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        // checks body
        for stmt in &schedule.body {
            match stmt {
                Stmt::Expr(expr) => match expr {
                    Expr::Identifier(callee_name) => {
                        local_context.systems.get(callee_name).unwrap_or_else(|| {
                            panic!("Calls a system that does not exist: {:?}", callee_name);
                        });
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

    fn check_function(&self, global_context: &TypeContext, function: &Function) {
        let mut local_context = TypeContext {
            components: global_context.components.clone(),
            resources: global_context.resources.clone(),
            systems: global_context.systems.clone(),
            schedule: global_context.schedule.clone(),
            functions: global_context.functions.clone(),
            variables: HashMap::new(),
        };

        // adds parameters to context
        for Param {
            name,
            param_type,
            mutable,
        } in &function.params
        {
            local_context
                .variables
                .insert(name.clone(), (param_type.clone(), mutable.clone()));
        }

        // checks body
        for stmt in &function.body {
            self.check_stmt(&mut local_context, stmt);
        }

        // checks return type
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

            Stmt::System(_) => (), // Déjà géré

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
