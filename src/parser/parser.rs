use std::collections::HashMap;

use crate::{lexer::Token, types::types::Type};

use super::ast::{BinaryOp, Expr, Function, Program, Stmt};

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
    pub fn parse_program(&mut self, in_block: bool) -> Result<Program, String> {
        let mut stmts = Vec::new();
        loop {
            // clone peeked token to avoid borrowing self across the loop
            let tok = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };

            // Arrêter seulement si c'est un bloc et qu'on trouve '}'
            if in_block && tok == Token::CurlyBraceClose {
                break;
            }

            if let Ok(stmt) = self.parse_statement() {
                stmts.push(stmt);
            } else {
                return Err(format!(
                    "Error during parsing for token {:?} (pos {})",
                    tok, self.pos
                ));
            }
        }

        if !in_block {
            let print_fn = self.functions.get("print").unwrap().clone();
            stmts.insert(0, Stmt::Function(print_fn));
        }

        Ok(Program { statements: stmts })
    }

    pub fn parse_statement(&mut self) -> Result<Stmt, String> {
        if let Some(token) = self.peek() {
            match token {
                Token::Fn => {
                    // prase function
                    self.next();
                    let name = match self.next().unwrap() {
                        Token::Identifier(name) => name,
                        _ => {
                            return Err(
                                "Expected an Identifier with the name of the function".into()
                            );
                        }
                    };
                    self.expect(Token::ParenthesisOpen).unwrap();

                    // parse parameters
                    let mut params = Vec::new();
                    while self.peek() != Some(&Token::ParenthesisClose) {
                        let param_name = match self.next().unwrap() {
                            Token::Identifier(name) => name,
                            _ => return Err("Expected an Identifier for function parameter".into()),
                        };
                        self.expect(Token::Colon).unwrap();
                        let param_type = Type::get_type(self.next().unwrap()).unwrap();
                        params.push((param_name, param_type));

                        if self.peek() == Some(&Token::Comma) {
                            self.next();
                        }
                    }
                    self.expect(Token::ParenthesisClose).unwrap();

                    // parse return type
                    let return_type = if self.peek() == Some(&Token::Arrow) {
                        self.expect(Token::Arrow).unwrap();
                        Type::get_type(self.next().unwrap()).unwrap()
                        // UTILISER self.parse_type()
                    } else {
                        Type::Null // Null if no return type
                    };

                    self.expect(Token::CurlyBraceOpen).unwrap();
                    let body = self.parse_program(true).unwrap();
                    self.expect(Token::CurlyBraceClose).unwrap();

                    let function = Function {
                        name: name.clone(),
                        params,
                        return_type,
                        body: body.statements,
                    };
                    self.functions.insert(name.clone(), function.clone());
                    Ok(Stmt::Function(function))
                }
                Token::Let => {
                    self.next();
                    let mut mutable = false;
                    if let Some(Token::Mut) = self.peek() {
                        mutable = true;
                        self.next();
                    }
                    if let Some(Token::Identifier(name)) = self.next() {
                        self.expect(Token::Assign).unwrap();
                        let expr = self.parse_expression(0).unwrap();
                        return Ok(Stmt::Let {
                            mutable,
                            name,
                            expression: expr,
                            expected_type: Type::UNDEFINED,
                        });
                    } else {
                        return Err("Expected an Identifier for assignment".into());
                    }
                }
                _ => self.parse_expression(0).map(Stmt::Expr),
            }
        } else {
            Err("No Token found".into())
        }
    }

    /// check if the next Token matches the expected one
    fn expect(&mut self, expected: Token) -> Result<(), String> {
        if let Some(tok) = self.next() {
            if tok == expected {
                return Ok(());
            }
        }
        Err("Next Token does not match the expected one".into())
    }

    fn parse_expression(&mut self, min_prec: u8) -> Result<Expr, String> {
        // parse atom
        let mut lhs = match self.next().unwrap() {
            Token::Number(n) => Expr::Number(n.parse().ok().unwrap()),
            Token::Identifier(id) => Expr::Identifier(id),
            Token::StringLiteral(string_literal) => Expr::StringLiteral(string_literal),
            Token::ParenthesisOpen => {
                let expr = self.parse_expression(0).unwrap();
                self.expect(Token::ParenthesisClose).unwrap();
                expr
            }
            _ => return Err("Unexpected Token".into()),
        };

        // get all parameters
        loop {
            if let Some(Token::ParenthesisOpen) = self.peek() {
                self.next();
                let mut args = Vec::new();
                if self.peek() != Some(&Token::ParenthesisClose) {
                    loop {
                        args.push(self.parse_expression(0)?);
                        if self.peek() == Some(&Token::Comma) {
                            self.next(); // consume ','
                            continue;
                        }
                        break;
                    }
                }
                self.expect(Token::ParenthesisClose).unwrap();
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
                let op = BinaryOp::get_binary_op(&self.next().unwrap())?;
                let next_min = if right_assoc { prec } else { prec + 1 };
                let rhs = self.parse_expression(next_min)?;
                lhs = Expr::BinaryOp {
                    left: Box::new(lhs),
                    op,
                    right: Box::new(rhs),
                };
            } else {
                break;
            }
        }
        Ok(lhs)
    }

    fn parse_type(&mut self) -> Result<Type, String> {
        if self.peek() == Some(&Token::Ampersand) {
            self.next();
            let mutable = if self.peek() == Some(&Token::Mut) {
                self.next();
                true
            } else {
                false
            };
            let inner_type = self.parse_type()?;
            Ok(Type::Reference {
                inner: Box::new(inner_type),
                mutable: mutable,
            })
        } else {
            Type::get_type(self.next().unwrap())
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
