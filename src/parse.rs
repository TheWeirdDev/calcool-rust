use crate::{
    expr::{Expr, Result},
    lex::{get_token_precedence, Token, TokenKind, TokenPrecedance},
};

pub struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens }
    }

    pub fn parse(&self) -> Result<Expr> {
        let mut pos = 0;
        let res = self.parse_impl(&mut pos, TokenPrecedance::None)?;
        if pos < self.tokens.len() {
            Err(format!("Unexpected token: {}", self.tokens[pos].value))
        } else {
            Ok(res)
        }
    }

    fn consume(&self, kind: TokenKind, pos: &mut usize) -> Result<&Token> {
        if let Some(token) = self.tokens.get(*pos) {
            if token.kind == kind {
                *pos += 1;
                Ok(token)
            } else {
                Err(format!("Expected {:?}, found {:?}", kind, token.kind))
            }
        } else {
            Err("Unexpected end of input".to_string())
        }
    }
    fn expect(&self, kind: TokenKind, pos: &mut usize) -> Result<()> {
        if let Some(token) = self.tokens.get(*pos) {
            if token.kind != kind {
                return Err(format!("Expected {:?}, found {:?}", kind, token.kind));
            }
            Ok(())
        } else {
            Err("Unexpected end of input".to_string())
        }
    }

    fn parse_impl(&self, pos: &mut usize, prec: TokenPrecedance) -> Result<Expr> {
        if *pos >= self.tokens.len() {
            return Err("Unexpected end of input".to_string());
        }
        let token = &self.tokens[*pos];
        *pos += 1;
        let mut left = match token.kind {
            TokenKind::Num(_) => Expr::Num(token.clone()),
            TokenKind::Op => Expr::Unary(
                token.value.clone(),
                Box::new(self.parse_impl(pos, TokenPrecedance::Unary)?),
            ),
            TokenKind::Open => {
                let res = self.parse_impl(pos, TokenPrecedance::None)?;
                self.consume(TokenKind::Close, pos)?;
                res
            }
            TokenKind::Id => {
                if let Some(t) = self.tokens.get(*pos) {
                    match t.kind {
                        TokenKind::Open => {
                            self.consume(TokenKind::Open, pos)?;
                            let mut args = Vec::new();
                            while self.tokens[*pos].kind != TokenKind::Close {
                                args.push(Box::new(self.parse_impl(pos, TokenPrecedance::None)?));
                                if self.tokens[*pos].kind == TokenKind::Comma {
                                    self.consume(TokenKind::Comma, pos)?;
                                } else {
                                    self.expect(TokenKind::Close, pos)?;
                                }
                            }
                            let res = Expr::Call(token.value.clone(), args);
                            self.consume(TokenKind::Close, pos)?;
                            res
                        }
                        TokenKind::Assign => {
                            self.consume(TokenKind::Assign, pos)?;
                            Expr::Assign(
                                token.value.clone(),
                                Box::new(self.parse_impl(pos, TokenPrecedance::None)?),
                            )
                        }
                        _ => Expr::Var(token.value.clone()),
                    }
                } else {
                    Expr::Var(token.value.clone())
                }
            }
            TokenKind::Define => {
                let name = self.consume(TokenKind::Id, pos)?;
                self.consume(TokenKind::Open, pos)?;
                let mut args = Vec::new();
                while self.tokens[*pos].kind != TokenKind::Close {
                    args.push(self.consume(TokenKind::Id, pos)?.value.clone());
                    if self.tokens[*pos].kind != TokenKind::Close {
                        self.consume(TokenKind::Comma, pos)?;
                    }
                }
                self.consume(TokenKind::Close, pos)?;
                self.consume(TokenKind::Assign, pos)?;
                let res = self.parse_impl(pos, TokenPrecedance::None)?;
                Expr::Define(name.value.clone(), args, Box::new(res))
            }
            _ => return Err(format!("Unexpected token {:?}", token.kind)),
        };
        while *pos < self.tokens.len() {
            if let Some(next) = self.tokens.get(*pos) {
                if next.kind == TokenKind::Op && prec < get_token_precedence(next) {
                    *pos += 1;
                    left = Expr::Binary(
                        next.value.clone(),
                        Box::new(left),
                        Box::new(self.parse_impl(pos, get_token_precedence(next))?),
                    );
                } else {
                    break;
                }
            }
        }
        return Ok(left);
    }
}
// pub fn parse(tokens: Vec<Token>) -> Expr {
//     let mut parser = Parser::new(tokens);
