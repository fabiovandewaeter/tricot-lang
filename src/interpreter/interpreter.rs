use crate::{ecs::ecs::World, parser::ast::*, types::types::Type, values::Value};
use std::collections::{HashMap, HashSet};

pub struct Interpreter {
    global_env: HashMap<String, Value>,
    stack: Vec<HashMap<String, Value>>,
    components: HashMap<String, Component>,
    systems: HashMap<String, System>,
    schedule: Option<Schedule>,
    functions: HashMap<String, Function>,
    saved_variables_for_tests: HashMap<String, Value>,
    world: World,
    // tracker les systèmes exécutés for systems that should only be called once
    executed_systems: HashSet<String>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            global_env: HashMap::new(),
            stack: vec![HashMap::new()],
            components: HashMap::new(),
            systems: HashMap::new(),
            schedule: None,
            functions: HashMap::new(),
            saved_variables_for_tests: HashMap::new(),
            world: World::new(),
            executed_systems: HashSet::new(),
        }
    }

    /// scheduler_loops: -1 for infinite loop
    pub fn run(&mut self, prog: Program, mut scheduler_loops: i32) {
        for stmt in &prog.statements {
            match stmt {
                Stmt::Component(comp) => {
                    self.components.insert(comp.name.clone(), comp.clone());
                }
                Stmt::System(system) => {
                    self.systems.insert(system.name.clone(), system.clone());
                }
                Stmt::Schedule(schedule) => {
                    self.schedule = Some(schedule.clone());
                }
                Stmt::Function(func) => {
                    self.functions.insert(func.name.clone(), func.clone());
                }
                _ => (),
            }
        }

        for stmt in prog.statements {
            match stmt {
                Stmt::Function(_) | Stmt::Component(_) | Stmt::System(_) | Stmt::Schedule(_) => {}
                _ => self.run_stmt(stmt),
            }
        }

        if let Some(mut schedule) = self.schedule.clone() {
            if scheduler_loops < 0 {
                loop {
                    self.run_schedule(&mut schedule);
                }
            } else {
                while scheduler_loops > 0 {
                    self.run_schedule(&mut schedule);
                    scheduler_loops -= 1;
                }
            }
        }
    }

    fn run_schedule(&mut self, schedule: &mut Schedule) {
        for stmt in &schedule.body {
            if let Stmt::Expr(Expr::CallSystem { callee, once }) = stmt {
                if let Expr::Identifier(called_system) = &**callee {
                    if *once {
                        // Vérifier dans l'état global de l'interpréteur
                        if !self.executed_systems.contains(called_system) {
                            self.call_system(called_system);
                            // Mettre à jour l'état global
                            self.executed_systems.insert(called_system.to_string());
                        }
                    } else {
                        self.call_system(called_system);
                    }
                } else {
                    panic!("Expected identifier as callee, got: {:?}", callee);
                }
            } else {
                panic!(
                    "Expected a system call expression in schedule body, got: {:?}",
                    stmt
                );
            }
        }
    }

    fn run_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Let {
                name, expression, ..
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
                    println!("=> {:?}", val);
                }
            }
            _ => {}
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
            Expr::CallSystem { callee: _, once: _ } => unreachable!(),
        }
    }

    pub fn get_variable(&self, name: &str) -> Value {
        for env in self.stack.iter().rev() {
            if let Some(val) = env.get(name) {
                return val.clone();
            }
        }
        self.global_env
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("Undefined identifier: {}", name))
    }

    fn set_variable(&mut self, name: &str, value: Value) {
        if self.stack.len() > 1 {
            if let Some(env) = self.stack.last_mut() {
                env.insert(name.to_string(), value);
                return;
            }
        }
        self.global_env.insert(name.to_string(), value);
    }

    fn get_variable_skip(&self, name: &str, skip: usize) -> Value {
        let stack_len = self.stack.len();
        if skip >= stack_len {
            self.global_env
                .get(name)
                .cloned()
                .unwrap_or_else(|| panic!("Undefined variable: {}", name))
        } else {
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
            if self.global_env.contains_key(name) {
                self.global_env.insert(name.to_string(), value);
                return;
            }
        } else {
            for env in self.stack.iter_mut().rev().skip(skip) {
                if env.contains_key(name) {
                    env.insert(name.to_string(), value.clone());
                    return;
                }
            }
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
                let mut base_value = self.eval_expr(base_expr.clone());
                if let Value::ComponentInstance { name, mut fields } = base_value {
                    fields.insert(field_name, value);
                    base_value = Value::ComponentInstance { name, fields };
                    self.assign_target(base_expr, base_value);
                } else {
                    panic!("Cannot assign field to non-component instance");
                }
            }
            _ => panic!("Invalid assignment target"),
        }
    }

    fn compound_assign(&mut self, target: Expr, op: BinaryOp, rhs: Value) {
        if let Expr::BinaryOp {
            left,
            op: BinaryOp::Dot,
            right,
        } = target
        {
            let field_expr = Expr::BinaryOp {
                left: left.clone(),
                op: BinaryOp::Dot,
                right: right.clone(),
            };
            let current_value = self.eval_expr(field_expr);
            let new_value = self.apply_binary_op(current_value, op, rhs);
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

        let (current_value, name, depth_at_creation, mutable) = match target {
            Expr::Identifier(name) => (self.get_variable(&name), name, 0, true),
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
        let mut components_to_insert = Vec::new();

        for init_expr in inits {
            let (comp_name, args) = match init_expr {
                Expr::Call { callee, args } => {
                    if let Expr::Identifier(name) = &**callee {
                        (name.clone(), args)
                    } else {
                        panic!("Expected component constructor identifier");
                    }
                }
                _ => panic!("Invalid spawn syntax"),
            };

            let comp_def = self
                .components
                .get(&comp_name)
                .unwrap_or_else(|| panic!("Unknown component: {}", comp_name))
                .clone();

            let mut field_values = HashMap::new();
            for (i, arg_expr) in args.iter().enumerate() {
                let val = self.eval_expr(arg_expr.clone());
                let field_name = match &comp_def.fields[i] {
                    Field::Named(name, _) => name.clone(),
                    Field::Unnamed(_) => panic!("Unnamed fields not supported in spawn"),
                };
                field_values.insert(field_name, val);
            }

            let instance = Value::ComponentInstance {
                name: comp_name.clone(),
                fields: field_values,
            };
            components_to_insert.push((comp_name.clone(), instance));
        }

        let new_id = self.world.spawn();
        for (comp_name, value) in components_to_insert {
            self.world.add_component(new_id, comp_name, value);
        }

        Value::Int(new_id as i64)
    }

    fn call_system(&mut self, name: &str) {
        let system = self.systems.get(name).unwrap().clone();

        if system.params.is_empty() {
            self.stack.push(HashMap::new());
            for stmt in &system.body {
                self.run_stmt(stmt.clone());
            }
            self.stack.pop();
            return;
        }

        let component_names: Vec<&str> = system
            .params
            .iter()
            .map(|param| match &param.param_type {
                Type::Component(name) => name.as_str(),
                _ => panic!("System parameter must be a component"),
            })
            .collect();

        let matching_entities = self.world.query_entities(&component_names);

        for entity_id in matching_entities {
            let mut frame = HashMap::new();

            for param in &system.params {
                let comp_name = match &param.param_type {
                    Type::Component(name) => name,
                    _ => unreachable!(),
                };

                if let Some(comp_value) =
                    self.world.get_component_mut(entity_id, comp_name.as_str())
                {
                    frame.insert(param.name.clone(), comp_value.clone());
                }
            }

            self.stack.push(frame);
            for stmt in &system.body {
                self.run_stmt(stmt.clone());
            }
            self.update_components(entity_id, &system);
            self.stack.pop();
        }
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

    fn update_components(&mut self, entity_id: u64, system: &System) {
        for param in &system.params {
            if param.mutable {
                // Récupère le nom du COMPOSANT (ex: "Position")
                let comp_name = match &param.param_type {
                    Type::Component(name) => name,
                    _ => panic!("Paramètre non-composant"),
                };

                if let Value::ComponentInstance { fields, .. } = self.get_variable(&param.name) {
                    // Utilise comp_name pour le monde
                    if let Some(comp_value) = self.world.get_component_mut(entity_id, comp_name) {
                        // ✅
                        *comp_value = Value::ComponentInstance {
                            name: comp_name.clone(), // ✅
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

        let evaluated_args: Vec<Value> = args.into_iter().map(|arg| self.eval_expr(arg)).collect();
        let mut frame = HashMap::new();

        for (param, value) in func.params.iter().zip(evaluated_args.iter()) {
            frame.insert(param.name.clone(), value.clone());
        }

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
