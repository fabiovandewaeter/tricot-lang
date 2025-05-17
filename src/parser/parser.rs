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
    pub fn parse_program(&mut self, in_block: bool) -> Option<Program> {
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

            if let Some(stmt) = self.parse_statement() {
                stmts.push(stmt);
            } else {
                eprintln!(
                    "Error during parsing for token {:?} (pos {})",
                    tok, self.pos
                );
                return None;
            }
        }
        Some(Program { statements: stmts })
    }

    // CHANGER LE TYPE DE RETOURN EN Result<Stmt, String> JE PENSE
    pub fn parse_statement(&mut self) -> Option<Stmt> {
        if let Some(Token::Let) = self.peek() {
            self.next();
            let mut mutable = false;
            if let Some(Token::Mut) = self.peek() {
                mutable = true;
                self.next();
            }
            if let Some(Token::Identifier(name)) = self.next() {
                self.expect(Token::Assign)?;
                let expr = self.parse_expression(0)?;
                return Some(Stmt::Let {
                    mutable,
                    name,
                    expression: expr,
                    expected_type: Type::UNDEFINED,
                });
            } else {
                return None; // erreur: identifiant attendu
            }
        } else if let Some(Token::Fn) = self.peek() {
            // prase function
            self.next();
            let name = match self.next()? {
                Token::Identifier(name) => name,
                _ => return None, // Erreur: nom attendu
            };
            self.expect(Token::ParenthesisOpen)?;

            // parse parameters
            let mut params = Vec::new();
            while self.peek() != Some(&Token::ParenthesisClose) {
                let param_name = match self.next()? {
                    Token::Identifier(name) => name,
                    _ => return None,
                };
                self.expect(Token::Colon)?;
                let param_type = Type::get_type(self.next()?).unwrap();
                params.push((param_name, param_type));

                if self.peek() == Some(&Token::Comma) {
                    self.next();
                }
            }
            self.expect(Token::ParenthesisClose)?;

            // parse return type
            let return_type = if self.peek() == Some(&Token::Arrow) {
                self.expect(Token::Arrow)?;
                Type::get_type(self.next()?).unwrap()
            } else {
                Type::Null // Null if no return type
            };

            self.expect(Token::CurlyBraceOpen)?;
            let body = self.parse_program(true)?;
            self.expect(Token::CurlyBraceClose)?;

            let function = Function {
                name: name.clone(),
                params,
                return_type,
                body: body.statements,
            };
            self.functions.insert(name.clone(), function.clone());
            Some(Stmt::Function(function))
        } else {
            self.parse_expression(0).map(Stmt::Expr)
        }
    }

    /// Vérifie que le token suivant correspond à `expected`, sinon None.
    fn expect(&mut self, expected: Token) -> Option<()> {
        if let Some(tok) = self.next() {
            if tok == expected {
                return Some(());
            }
        }
        None
    }

    /// Parser d'expression de type Pratt pour gérer la priorité.
    fn parse_expression(&mut self, min_prec: u8) -> Option<Expr> {
        // parse atom
        let mut lhs = match self.next()? {
            Token::Number(n) => Expr::Number(n.parse().ok()?),
            Token::Identifier(id) => Expr::Identifier(id),
            Token::StringLiteral(string_literal) => Expr::StringLiteral(string_literal),
            Token::ParenthesisOpen => {
                let expr = self.parse_expression(0)?;
                self.expect(Token::ParenthesisClose)?;
                expr
            }
            _ => return None,
        };

        // 2) suffixe d’appels : tant qu’on voit une parenthèse, on collecte args
        loop {
            if let Some(Token::ParenthesisOpen) = self.peek() {
                self.next(); // consume '('
                let mut args = Vec::new();
                // s’il n’y a pas tout de suite ')', on parse des args séparés par des ','
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
                self.expect(Token::ParenthesisClose)?;
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
                let op = match self.next()? {
                    Token::Plus => BinaryOp::Plus,
                    Token::Minus => BinaryOp::Minus,
                    Token::Star => BinaryOp::Star,
                    Token::Slash => BinaryOp::Slash,
                    _ => unreachable!(),
                };
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
        Some(lhs)
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
