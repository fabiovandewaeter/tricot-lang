use std::collections::{HashMap, HashSet};

use crate::{
    interpreter::ecs_registry::EcsRegistry, parser::ast::*, types::types::Type, values::Value,
};

pub struct Interpreter {
    global_env: HashMap<String, Value>,
    stack: Vec<HashMap<String, Value>>,
    components: HashMap<String, Component>,
    systems: HashMap<String, System>,
    functions: HashMap<String, Function>,
    ecs_registry: EcsRegistry,
    saved_variables_for_tests: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            global_env: HashMap::new(),
            stack: vec![HashMap::new()],
            components: HashMap::new(),
            systems: HashMap::new(),
            functions: HashMap::new(),
            ecs_registry: EcsRegistry::new(),
            saved_variables_for_tests: HashMap::new(),
        }
    }

    pub fn run(&mut self, prog: Program) {
        // Register functions first
        for stmt in &prog.statements {
            match stmt {
                Stmt::Component(comp) => {
                    self.components.insert(comp.name.clone(), comp.clone());
                }

                Stmt::System(system) => {
                    self.systems.insert(system.name.clone(), system.clone());
                }

                Stmt::Function(func) => {
                    self.functions.insert(func.name.clone(), func.clone());
                }

                _ => (),
            }
        }

        // Execute statements
        for stmt in prog.statements {
            match stmt {
                Stmt::Function(_) | Stmt::Component(_) | Stmt::System(_) => {}

                Stmt::Schedule(schedule) => {
                    self.run_schedule(&schedule);
                }

                _ => {
                    self.run_stmt(stmt);
                }
            }
        }
    }

    fn run_schedule(&mut self, schedule: &Schedule) {
        for stmt in &schedule.body {
            if let Stmt::Expr(Expr::Identifier(called_system)) = stmt {
                self.call_system(&called_system);
            } else {
                panic!(
                    "Expected an identifier expression to call a system in the schedule body, but found: {:?}",
                    stmt
                )
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

            Expr::Spawn(inits) => self.eval_spawn(&inits),
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

            Expr::BinaryOp {
                left,
                op: BinaryOp::Dot,
                right,
            } => {
                let base_expr = *left;
                let field_name = match *right {
                    Expr::Identifier(name) => name,
                    _ => panic!("Field name must be an identifier"),
                };

                // Get the base value (component instance)
                let mut base_value = self.eval_expr(base_expr.clone());

                // Update the field in the component instance
                if let Value::ComponentInstance { name, mut fields } = base_value {
                    fields.insert(field_name, value);
                    base_value = Value::ComponentInstance { name, fields };
                } else {
                    panic!("Cannot assign field to non-component instance");
                }

                // Assign the updated component back to the base
                self.assign_target(base_expr, base_value);
            }

            _ => panic!("Invalid assignment target"),
        }
    }

    fn compound_assign(&mut self, target: Expr, op: BinaryOp, rhs: Value) {
        // Handle field access separately
        if let Expr::BinaryOp {
            left,
            op: BinaryOp::Dot,
            right,
        } = target
        {
            // Evaluate the current value of the field
            let field_expr = Expr::BinaryOp {
                left: left.clone(),
                op: BinaryOp::Dot,
                right: right.clone(),
            };
            let current_value = self.eval_expr(field_expr);
            let new_value = self.apply_binary_op(current_value, op, rhs);

            // Assign the new value to the field
            self.assign_target(
                Expr::BinaryOp {
                    left,
                    op: BinaryOp::Dot,
                    right,
                },
                new_value,
            );
            return;
        }

        // Handle other cases
        let (current_value, name, depth_at_creation, mutable) = match target {
            Expr::Identifier(name) => {
                let val = self.get_variable(&name);
                (val, name, 0, true)
            }

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
                    let val = self.get_variable_skip(&name, skip);
                    (val, name, depth_at_creation, mutable)
                } else {
                    panic!("Cannot dereference non-reference value");
                }
            }

            other => panic!("Invalid compound assignment target: {:?}", other),
        };

        let new_value = self.apply_binary_op(current_value, op, rhs);

        match (name, depth_at_creation, mutable) {
            (name, 0, _) => self.set_variable(&name, new_value),
            (name, depth, _) => {
                let skip = self.stack.len() - depth;
                self.update_variable_skip(&name, new_value, skip);
            }
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
        match op {
            BinaryOp::Dot => {
                // Handle field access on component instances
                let base_value = self.eval_expr(left);
                match base_value {
                    Value::ComponentInstance { fields, .. } => {
                        if let Expr::Identifier(field_name) = right {
                            fields
                                .get(&field_name)
                                .cloned()
                                .unwrap_or_else(|| panic!("Field '{}' not found", field_name))
                        } else {
                            panic!("Right-hand side of dot must be an identifier");
                        }
                    }
                    _ => panic!("Cannot access field on non-component value"),
                }
            }
            _ => {
                let lval = self.eval_expr(left);
                let rval = self.eval_expr(right);
                self.apply_binary_op(lval, op, rval)
            }
        }
    }

    fn eval_spawn(&mut self, inits: &Vec<Expr>) -> Value {
        // 1. Construire la liste des composants à attacher
        //    -> Vec<(nom_du_composant, valeur_composant)>
        let mut components_to_insert: Vec<(String, Value)> = Vec::new();

        for init_expr in inits {
            // Chaque `init_expr` doit être un `Expr::Call { callee, args }`
            let (comp_name, args) = match init_expr {
                Expr::Call { callee, args } => {
                    // Vérifier que `callee` est un identifiant
                    if let Expr::Identifier(name) = &**callee {
                        (name.clone(), args)
                    } else {
                        panic!(
                            "Dans spawn, attendu un constructeur de composant (Ident), mais trouvé : {:?}",
                            callee
                        );
                    }
                }
                other => panic!(
                    "Chaque élément de `spawn {{ … }}` doit être un appel de composant. Trouvé : {:?}",
                    other
                ),
            };

            // 2. Vérifier que ce composant existe bien dans l'AST (self.components)
            let comp_def = self
                .components
                .get(&comp_name)
                .cloned()
                .unwrap_or_else(|| panic!("Composant inconnu : {}", comp_name));

            // 3. Evaluer chaque argument pour obtenir un Value
            if args.len() != comp_def.fields.len() {
                panic!(
                    "Le composant '{}' attend {} champ(s), mais le spawn en fournit {}",
                    comp_name,
                    comp_def.fields.len(),
                    args.len()
                );
            }

            // 4. Construire `field_values: HashMap<String, Value>`
            let mut field_values: HashMap<String, Value> = HashMap::new();
            for (i, arg_expr) in args.iter().enumerate() {
                // On évalue l'argument
                let val = self.eval_expr(arg_expr.clone());

                // On récupère le nom du i-ème champ dans la définition AST du composant
                let field_name = match &comp_def.fields[i] {
                    Field::Named(fname, _ty) => fname.clone(),
                    Field::Unnamed(_) => {
                        panic!(
                            "Dans votre définition de composant '{}', il y a des champs Unnamed. \
                             Pour spawn, tous les champs doivent être `Field::Named`.",
                            comp_name
                        )
                    }
                };

                // On associe le nom du champ à la valeur évaluée
                field_values.insert(field_name, val);
            }

            // 5. Créer un `Value::ComponentInstance` et l'ajouter à la liste
            let instance = Value::ComponentInstance {
                name: comp_name.clone(),
                fields: field_values,
            };
            components_to_insert.push((comp_name.clone(), instance));
        }

        // 6. Appeler `spawn_entity` dans l'EcsRegistry
        let new_id = self.ecs_registry.spawn_entity(components_to_insert);

        // 7. Retourner l'ID sous forme d'entier
        Value::Int(new_id as i64)
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

    fn call_system(&mut self, name: &str) {
        let system = self
            .systems
            .get(name)
            .expect(&format!("Undefined system: {}", name))
            .clone();

        // if no parameters in query, execute only one time
        if system.params.is_empty() {
            // On pousse un frame vide (le système n’a pas de variables liées à une entité)
            self.stack.push(HashMap::new());
            for stmt in &system.body {
                self.run_stmt(stmt.clone());
            }
            self.stack.pop();
            return;
        }

        // else, find corresponding components
        let component_types: Vec<&str> = system
            .params
            .iter()
            .map(|p| match &p.param_type {
                Type::Component(name) => name.as_str(),
                other => panic!("Type not found: {:?}", other),
            })
            .collect();

        let mut matching_entities = self.find_entities_with_components(&component_types);

        // execute system on entities
        for entity_id in matching_entities {
            let mut frame = HashMap::new();

            for param in &system.params {
                let comp_name = match &param.param_type {
                    Type::Component(name) => name,
                    _ => unreachable!(),
                };

                if let Some(comp_value) = self.ecs_registry.get_component_mut(entity_id, comp_name)
                {
                    frame.insert(param.name.clone(), comp_value.clone());
                }
            }

            self.stack.push(frame);
            //self.run_system_body(&system.body);
            for stmt in &system.body {
                self.run_stmt(stmt.clone());
            }

            self.update_components(entity_id, &system);
            self.stack.pop();
        }
    }

    // Trouve les entités ayant tous les composants requis
    fn find_entities_with_components(&self, component_types: &[&str]) -> HashSet<u64> {
        let mut result = HashSet::new();

        if let Some(first_type) = component_types.first() {
            if let Some(entities) = self.ecs_registry.components.get(*first_type) {
                result.extend(entities.keys().cloned());
            }

            for comp_type in &component_types[1..] {
                if let Some(entities) = self.ecs_registry.components.get(*comp_type) {
                    result.retain(|id| entities.contains_key(id));
                }
            }
        }

        result
    }

    // updates components after running the system
    fn update_components(&mut self, entity_id: u64, system: &System) {
        for param in &system.params {
            if param.mutable {
                if let Value::ComponentInstance { fields, .. } = self.get_variable(&param.name) {
                    if let Some(comp_value) =
                        self.ecs_registry.get_component_mut(entity_id, &param.name)
                    {
                        *comp_value = Value::ComponentInstance {
                            name: param.name.clone(),
                            fields: fields.clone(),
                        };
                    }
                }
            }
        }
    }

    fn call_function(&mut self, name: &str, args: Vec<Expr>) -> Value {
        match name {
            "print" => self.call_print(args),
            "save_variable_for_tests" => self.call_save_variable_for_tests(args),
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

    fn call_save_variable_for_tests(&mut self, args: Vec<Expr>) -> Value {
        if let Value::String(variable_name) = self.eval_expr(args[0].clone()) {
            let value = self.eval_expr(args[1].clone());
            self.save_for_variable_tests(variable_name, value);
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

    // for tests
    pub fn save_for_variable_tests(&mut self, variable_name: String, value: Value) {
        self.saved_variables_for_tests.insert(variable_name, value);
    }

    pub fn get_saved_for_tests(&self, variable_name: String) -> Value {
        self.saved_variables_for_tests
            .get(&variable_name)
            .unwrap()
            .clone()
    }
}
