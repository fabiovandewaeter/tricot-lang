use crate::lexer::Token;

use super::ast::{BinaryOp, Expr, Program, Stmt};

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
        if let Some(tok) = self.tokens.get(self.pos).cloned() {
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    /// Parse all statements until EOF or closing brace
    pub fn parse_program(&mut self) -> Option<Program> {
        let mut stmts = Vec::new();
        loop {
            // clone peeked token to avoid borrowing self across the loop
            let tok = match self.peek() {
                Some(t) => t.clone(),
                None => break,
            };
            if tok == Token::CurlyBraceClose {
                break;
            }
            if let Some(stmt) = self.parse_statement() {
                stmts.push(stmt);
            } else {
                eprintln!("Erreur de parsing au token {:?} (pos {})", tok, self.pos);
                return None;
            }
        }
        Some(Program { statements: stmts })
    }

    pub fn parse_statement(&mut self) -> Option<Stmt> {
        if let Some(Token::Let) = self.peek() {
            self.next(); // consume 'let'
            if let Some(Token::Identifier(name)) = self.next() {
                self.expect(Token::Assign)?;
                let expr = self.parse_expression(0)?;
                return Some(Stmt::Let { name, value: expr });
            } else {
                return None; // erreur: identifiant attendu
            }
        }
        // sinon, on parse une expression
        self.parse_expression(0).map(Stmt::Expr)
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
            Token::ParenthesisOpen => {
                let expr = self.parse_expression(0)?;
                self.expect(Token::ParenthesisClose)?;
                expr
            }
            _ => return None,
        };

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
}

/// Définition des priorités : plus le nombre est élevé, plus la liaison est forte.
fn precedence(tok: &Token) -> Option<(u8, bool)> {
    match tok {
        Token::Plus | Token::Minus => Some((1, false)),
        Token::Star | Token::Slash => Some((2, false)),
        _ => None,
    }
}
