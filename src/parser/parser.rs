use std::collections::HashMap;

use crate::{lexer::Token, types::types::Type};

use super::ast::{BinaryOp, Expr, Function, Program, Stmt, UnaryOp};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    functions: HashMap<String, Function>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let print_function = Function {
            name: "print".into(),
            params: vec![("value".into(), Type::String)],
            return_type: Type::Null,
            body: Vec::new(),
        };
        let mut functions = HashMap::new();
        functions.insert("print".into(), print_function);
        Self {
            tokens,
            pos: 0,
            functions,
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn next(&mut self) -> Option<Token> {
        if let Some(tok) = self.tokens.get(self.pos).cloned() {
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    /// Parse all statements until EOF or closing brace
    pub fn parse_program(&mut self, in_block: bool) -> Program {
        let mut stmts = Vec::new();
        loop {
            // clone peeked token to avoid borrowing self across the loop
            let tok = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };

            // stops when it reachs a '}'
            if in_block && tok == Token::CurlyBraceClose {
                break;
            }

            stmts.push(self.parse_statement());
        }

        if !in_block {
            // add the print function to the list of known functions by the Program
            let print_fn = self.functions.get("print").unwrap().clone();
            stmts.insert(0, Stmt::Function(print_fn));
        }

        Program { statements: stmts }
    }

    pub fn parse_statement(&mut self) -> Stmt {
        if let Some(token) = self.peek() {
            match token {
                Token::Fn => {
                    // prase function
                    self.next();
                    let name = match self.next().unwrap() {
                        Token::Identifier(name) => name,
                        _ => {
                            panic!("Expected an Identifier with the name of the function");
                        }
                    };
                    self.expect(Token::ParenthesisOpen);

                    // parse parameters
                    let mut params = Vec::new();
                    while self.peek() != Some(&Token::ParenthesisClose) {
                        let param_name = match self.next().unwrap() {
                            Token::Identifier(name) => name,
                            _ => panic!("Expected an Identifier for function parameter"),
                        };
                        self.expect(Token::Colon);
                        //let param_type = Type::get_type(self.next().unwrap()).unwrap();
                        let param_type = self.parse_type();
                        params.push((param_name, param_type));

                        if self.peek() == Some(&Token::Comma) {
                            self.next();
                        }
                    }
                    self.expect(Token::ParenthesisClose);

                    // parse return type
                    let return_type = if self.peek() == Some(&Token::Arrow) {
                        self.expect(Token::Arrow);
                        //Type::get_type(self.next().unwrap()).unwrap()
                        self.parse_type()
                    } else {
                        Type::Null // Null if no return type
                    };

                    self.expect(Token::CurlyBraceOpen);
                    let body = self.parse_program(true);
                    self.expect(Token::CurlyBraceClose);

                    let function = Function {
                        name: name.clone(),
                        params,
                        return_type,
                        body: body.statements,
                    };
                    self.functions.insert(name.clone(), function.clone());
                    Stmt::Function(function)
                }

                Token::Let => {
                    self.next();
                    let mut mutable = false;
                    if let Some(Token::Mut) = self.peek() {
                        mutable = true;
                        self.next();
                    }
                    if let Some(Token::Identifier(name)) = self.next() {
                        self.expect(Token::Assign);
                        let expr = self.parse_expression(0);
                        return Stmt::Let {
                            mutable,
                            name,
                            expression: expr,
                            expected_type: Type::UNDEFINED,
                        };
                    } else {
                        panic!("Expected an Identifier for assignment");
                    }
                }

                _ => {
                    // get the expression
                    let expr = self.parse_expression(0);
                    // if token is a Token::Assign it's an assignmnent
                    if self.peek() == Some(&Token::Assign) {
                        self.next();
                        let rhs = self.parse_expression(0);
                        return Stmt::Assignment {
                            target: expr,
                            expression: rhs,
                        };
                    }
                    // else it's just an expression
                    Stmt::Expr(expr)
                }
            }
        } else {
            panic!("No Token found")
        }
    }

    /// check if the next Token matches the expected one
    fn expect(&mut self, expected: Token) {
        if let Some(tok) = self.next() {
            if tok == expected {
                return;
            }
        }
        panic!("Next Token does not match the expected one")
    }

    fn parse_expression(&mut self, min_prec: u8) -> Expr {
        // parse atom
        let mut lhs = match self.next().unwrap() {
            Token::Number(n) => Expr::Number(n.parse().ok().unwrap()),

            Token::Identifier(id) => Expr::Identifier(id),

            Token::StringLiteral(string_literal) => Expr::StringLiteral(string_literal),

            Token::Ampersand => {
                //self.next();
                let mutable = if self.peek() == Some(&Token::Mut) {
                    self.next();
                    true
                } else {
                    false
                };
                let inner = self.parse_expression(3);
                Expr::UnaryOp {
                    op: UnaryOp::AddrOf(mutable),
                    expr: Box::new(inner),
                }
            }

            Token::Star => {
                //self.next();
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

            token => panic!("{}", format!("Unexpected Token : {:?}", token)),
        };

        // get all parameters if it is a function call
        loop {
            if let Some(Token::ParenthesisOpen) = self.peek() {
                self.next();
                let mut args = Vec::new();
                if self.peek() != Some(&Token::ParenthesisClose) {
                    loop {
                        args.push(self.parse_expression(0));
                        if self.peek() == Some(&Token::Comma) {
                            self.next();
                            continue;
                        }
                        break;
                    }
                }
                self.expect(Token::ParenthesisClose);
                lhs = Expr::Call {
                    callee: Box::new(lhs),
                    args,
                };
            } else {
                break;
            }
        }

        // boucle pour les opérateurs de plus faible à plus fort
        while let Some(op_tok) = self.peek() {
            if let Some((prec, right_assoc)) = precedence(op_tok) {
                if prec < min_prec {
                    break;
                }
                let op = BinaryOp::get_binary_op(&self.next().unwrap()).unwrap();
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

    fn parse_type(&mut self) -> Type {
        if self.peek() == Some(&Token::Ampersand) {
            self.next();
            let mutable = if self.peek() == Some(&Token::Mut) {
                self.next();
                true
            } else {
                false
            };
            let inner_type = self.parse_type();
            Type::Reference {
                inner: Box::new(inner_type),
                mutable: mutable,
            }
        } else {
            Type::get_type(self.next().unwrap()).unwrap()
        }
    }
}

/// Définition des priorités : plus le nombre est élevé, plus la liaison est forte.
fn precedence(tok: &Token) -> Option<(u8, bool)> {
    match tok {
        Token::Plus | Token::Minus => Some((1, false)),
        Token::Star | Token::Slash => Some((2, false)),
        _ => None,
    }
}
