use crate::{
    lexer::Token,
    parser::ast::Schedule,
    types::types::{ParamContext, Type},
};

use super::ast::{
    BinaryOp, Component, Expr, Field, Function, Param, Program, Resource, Stmt, System, UnaryOp,
};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.get(self.pos).cloned().map(|tok| {
            self.pos += 1;
            tok
        })
    }

    pub fn parse_program(&mut self) -> Program {
        // the whole program is like a giant block of Stmt
        let stmts = self.parse_block();

        Program { statements: stmts }
    }

    fn parse_block(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while let Some(tok) = self.peek() {
            if *tok == Token::CurlyBraceClose {
                break;
            }
            stmts.push(self.parse_statement());
        }

        stmts
    }

    fn parse_statement(&mut self) -> Stmt {
        match self.peek() {
            Some(Token::Component) => self.parse_component(),
            Some(Token::Resource) => self.parse_resource(),
            Some(Token::System) => self.parse_system(),
            Some(Token::Schedule) => self.parse_schedule(),
            Some(Token::Fn) => self.parse_function(),
            Some(Token::Let) => self.parse_variable_declaration(),
            _ => self.parse_expression_or_assignment(),
        }
    }

    // ----------------- ECS -----------------
    fn parse_component(&mut self) -> Stmt {
        self.expect(Token::Component);
        let name = self.expect_identifier("component name");

        let fields = self.parse_fields();

        Stmt::Component(Component {
            name: name.clone(),
            fields,
        })
    }

    fn parse_resource(&mut self) -> Stmt {
        self.expect(Token::Resource);
        let name = self.expect_identifier("resource name");

        let fields = self.parse_fields();

        Stmt::Resource(Resource {
            name: name.clone(),
            fields,
        })
    }

    fn parse_fields(&mut self) -> Vec<Field> {
        self.expect(Token::ParenthesisOpen);
        let mut fields = Vec::new();

        while self.peek() != Some(&Token::ParenthesisClose) {
            let field = if let Token::Identifier(name) = self.peek().cloned().unwrap() {
                self.next();
                self.expect(Token::Colon);
                Field::Named(name.clone(), self.parse_type())
            } else {
                Field::Unnamed(self.parse_type())
            };

            fields.push(field);

            if !self.consume_if(Token::Comma) {
                break;
            }
        }

        self.expect(Token::ParenthesisClose);
        fields
    }

    fn parse_system(&mut self) -> Stmt {
        self.expect(Token::System);
        let name = self.expect_identifier("system name");

        // components
        let params = self.parse_parameters(ParamContext::Component);

        // resources
        let mut resources = Vec::new();
        if self.consume_if(Token::Using) {
            resources = self.parse_parameters(ParamContext::Resource);
        }

        self.expect(Token::CurlyBraceOpen);
        let body = self.parse_block();
        self.expect(Token::CurlyBraceClose);

        let system = System {
            name: name.clone(),
            params,
            resources,
            body: body,
        };

        Stmt::System(system)
    }

    fn parse_schedule(&mut self) -> Stmt {
        self.expect(Token::Schedule);

        self.expect(Token::CurlyBraceOpen);
        let mut systems = Vec::new();
        loop {
            match self.peek() {
                Some(Token::Identifier(name)) => {
                    // On consomme l’identifiant et on le stocke
                    let sys_name = name.clone();
                    self.next();
                    systems.push(sys_name);
                }
                other => {
                    panic!(
                        "Dans le bloc `schedule`, on attendait un identifiant de système, trouvé {:?}",
                        other
                    );
                }
            }

            // Si on trouve une virgule, on la consomme et on continue la boucle
            if self.consume_if(Token::Comma) {
                continue;
            } else {
                // Sinon, on sort de la boucle (fin de liste)
                break;
            }
        }
        self.expect(Token::CurlyBraceClose);

        // create Scredule body
        let mut body = Vec::new();
        for name in systems {
            // Ici, on crée un expr « entity_spawner » ou « move_entities »
            let expr_ident = Expr::Identifier(name);
            body.push(Stmt::Expr(expr_ident));
        }

        Stmt::Schedule(Schedule { body: body })
    }
    // ---------------------------------------

    fn parse_function(&mut self) -> Stmt {
        self.expect(Token::Fn);
        let name = self.expect_identifier("function name");

        let params = self.parse_parameters(ParamContext::Struct);
        let return_type = self.parse_return_type();

        self.expect(Token::CurlyBraceOpen);
        let body = self.parse_block();
        self.expect(Token::CurlyBraceClose);

        let function = Function {
            name: name.clone(),
            params,
            return_type,
            body: body,
        };

        Stmt::Function(function)
    }

    fn parse_parameters(&mut self, param_context: ParamContext) -> Vec<Param> {
        self.expect(Token::ParenthesisOpen);
        let mut params = Vec::new();

        while self.peek() != Some(&Token::ParenthesisClose) {
            let name = self.expect_identifier("function parameter");
            self.expect(Token::Colon);
            let mutable = if self.consume_if(Token::Mut) {
                true
            } else {
                false
            };
            let param_type = match self.parse_type() {
                Type::Unresolved(name) => match param_context {
                    ParamContext::Component => Type::Component(name),
                    ParamContext::Resource => Type::Resource(name),
                    ParamContext::Struct => Type::Struct(name),
                },
                other => other,
            };
            params.push(Param {
                name,
                mutable,
                param_type,
            });

            if !self.consume_if(Token::Comma) {
                break;
            }
        }

        self.expect(Token::ParenthesisClose);
        params
    }

    fn parse_return_type(&mut self) -> Type {
        if self.consume_if(Token::Arrow) {
            self.parse_type()
        } else {
            Type::Null
        }
    }

    fn parse_variable_declaration(&mut self) -> Stmt {
        self.expect(Token::Let);
        let mutable = self.consume_if(Token::Mut);
        let name = self.expect_identifier("variable name");

        self.expect(Token::Assign);
        let expr = self.parse_expression(0);

        Stmt::Let {
            mutable,
            name,
            expression: expr,
            expected_type: Type::UNDEFINED,
        }
    }

    fn parse_expression_or_assignment(&mut self) -> Stmt {
        let expr = self.parse_expression(0);

        match self.peek() {
            Some(Token::Assign) => {
                self.next();
                let rhs = self.parse_expression(0);
                Stmt::Assignment {
                    target: expr,
                    expression: rhs,
                }
            }
            Some(Token::AssignPlus) => self.parse_compound_assignment(expr, BinaryOp::Plus),
            Some(Token::AssignMinus) => self.parse_compound_assignment(expr, BinaryOp::Minus),
            Some(Token::AssignStar) => self.parse_compound_assignment(expr, BinaryOp::Star),
            Some(Token::AssignSlash) => self.parse_compound_assignment(expr, BinaryOp::Slash),
            _ => Stmt::Expr(expr),
        }
    }

    fn parse_compound_assignment(&mut self, target: Expr, op: BinaryOp) -> Stmt {
        self.next();
        let rhs = self.parse_expression(0);
        Stmt::CompoundAssignment {
            target,
            op,
            expression: rhs,
        }
    }

    fn parse_expression(&mut self, min_prec: u8) -> Expr {
        let mut lhs = self.parse_atom();

        // Handle function calls
        while let Some(Token::ParenthesisOpen) = self.peek() {
            self.next();
            let mut args = Vec::new();

            while self.peek() != Some(&Token::ParenthesisClose) {
                args.push(self.parse_expression(0));
                if !self.consume_if(Token::Comma) {
                    break;
                }
            }

            self.expect(Token::ParenthesisClose);
            lhs = Expr::Call {
                callee: Box::new(lhs),
                args,
            };
        }

        // Handle binary operators
        while let Some(tok) = self.peek() {
            if let Some((prec, right_assoc)) = precedence(tok) {
                if prec < min_prec {
                    break;
                }

                let op_token = self.next().unwrap();
                let op = BinaryOp::get_binary_op(&op_token).unwrap();
                let next_min = if right_assoc { prec } else { prec + 1 };
                let rhs = self.parse_expression(next_min);

                lhs = Expr::BinaryOp {
                    left: Box::new(lhs),
                    op,
                    right: Box::new(rhs),
                };
            } else {
                break;
            }
        }
        lhs
    }

    fn parse_atom(&mut self) -> Expr {
        match self.next().unwrap() {
            Token::Number(n) => Expr::Number(n.parse().unwrap()),

            Token::Identifier(id) => Expr::Identifier(id),

            Token::StringLiteral(s) => Expr::StringLiteral(s),

            Token::Ampersand => {
                let mutable = self.consume_if(Token::Mut);
                let inner = self.parse_expression(3);
                Expr::UnaryOp {
                    op: UnaryOp::AddrOf(mutable),
                    expr: Box::new(inner),
                }
            }

            Token::Star => {
                let inner = self.parse_expression(3);
                Expr::UnaryOp {
                    op: UnaryOp::Deref,
                    expr: Box::new(inner),
                }
            }

            Token::ParenthesisOpen => {
                let expr = self.parse_expression(0);
                self.expect(Token::ParenthesisClose);
                expr
            }

            Token::Spawn => {
                // On s’attend à trouver une accolade ouvrante
                self.expect(Token::CurlyBraceOpen);

                // On va collecter une liste d’expressions à l’intérieur de {...}
                let mut comps = Vec::new();
                while self.peek() != Some(&Token::CurlyBraceClose) {
                    // On parse ici n’importe quelle expression (typiquement un constructeur de composant,
                    // par exemple Position(0, 0) ) :
                    let comp_expr = self.parse_expression(0);
                    comps.push(comp_expr);

                    // Si une virgule suit, on la consomme et on continue
                    if self.consume_if(Token::Comma) {
                        continue;
                    } else {
                        break;
                    }
                }

                // On ferme l’accolade
                self.expect(Token::CurlyBraceClose);

                // On retourne une Expr::Spawn contenant la liste des composants
                Expr::Spawn(comps)
            }

            token => panic!("Unexpected token: {:?}", token),
        }
    }

    fn parse_type(&mut self) -> Type {
        if self.consume_if(Token::Ampersand) {
            let mutable = self.consume_if(Token::Mut);
            let inner_type = self.parse_type();
            Type::Reference {
                inner: Box::new(inner_type),
                mutable,
            }
        } else {
            Type::get_type(self.next().unwrap()).unwrap()
        }
    }

    // Utility functions
    fn expect_identifier(&mut self, ctx: &str) -> String {
        match self.next() {
            Some(Token::Identifier(name)) => name,
            _ => panic!("Expected identifier for {}", ctx),
        }
    }

    fn expect(&mut self, expected: Token) {
        match self.next() {
            Some(tok) if tok == expected => (),
            Some(tok) => panic!("Expected {:?}, found {:?}", expected, tok),
            None => panic!("Expected {:?}, but no token left", expected),
        }
    }

    fn consume_if(&mut self, token: Token) -> bool {
        if self.peek() == Some(&token) {
            self.next();
            true
        } else {
            false
        }
    }
}

fn precedence(tok: &Token) -> Option<(u8, bool)> {
    match tok {
        Token::Plus | Token::Minus => Some((1, false)),
        Token::Star | Token::Slash => Some((2, false)),
        Token::Dot => Some((3, false)),
        _ => None,
    }
}
