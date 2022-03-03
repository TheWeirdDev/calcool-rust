use crate::expr::{Result, Unit};

#[derive(Debug, PartialEq, Clone, Eq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Num(Unit),
    Op,
    Open,
    Close,
    Comma,
    Id,
    Assign,
    Define,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub loc: Location,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
pub enum TokenPrecedance {
    None = 0,
    AddSub,
    MulDiv,
    Pow,
    Unary,
    Group,
}

pub fn get_token_precedence(token: &Token) -> TokenPrecedance {
    match token.kind {
        TokenKind::Num(_) => TokenPrecedance::None,
        TokenKind::Op => match token.value.as_str() {
            "+" | "-" => TokenPrecedance::AddSub,
            "*" | "/" => TokenPrecedance::MulDiv,
            "^" => TokenPrecedance::Pow,
            _ => panic!("Unknown operator: {}", token.value),
        },
        TokenKind::Open => TokenPrecedance::Group,
        _ => panic!("Unknown token kind: {:?}", token.kind),
    }
}

#[derive(Debug, PartialEq)]
pub struct Lexer {
    input: Vec<char>,
    current_loc: Location,
    pos: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            input: input.chars().collect(),
            current_loc: Location { line: 1, column: 1 },
            pos: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos).cloned()
    }

    fn next(&mut self) -> Option<char> {
        let ch = self.peek();
        if let Some(c) = ch {
            if c == '\n' {
                self.current_loc.line += 1;
                self.current_loc.column = 1;
            } else {
                self.current_loc.column += 1;
            }
            self.pos += 1;
        }
        ch
    }

    pub fn lex(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        let len = self.input.len();
        while self.pos < len {
            self.skip_whitespace();

            if let Some(c) = self.peek() {
                match c {
                    '0'..='9' => {
                        tokens.push(self.read_number()?);
                    }
                    '+' | '-' | '*' | '/' | '^' => {
                        tokens.push(Token {
                            kind: TokenKind::Op,
                            value: c.to_string(),
                            loc: self.current_loc.clone(),
                        });
                        self.next();
                    }
                    ',' => {
                        tokens.push(Token {
                            kind: TokenKind::Comma,
                            value: c.to_string(),
                            loc: self.current_loc.clone(),
                        });
                        self.next();
                    }
                    '(' => {
                        tokens.push(Token {
                            kind: TokenKind::Open,
                            value: c.to_string(),
                            loc: self.current_loc.clone(),
                        });
                        self.next();
                    }
                    ')' => {
                        tokens.push(Token {
                            kind: TokenKind::Close,
                            value: c.to_string(),
                            loc: self.current_loc.clone(),
                        });
                        self.next();
                    }
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let mut id = self.read_id();
                        match id.value.to_ascii_lowercase().as_str() {
                            "def" => {
                                id.kind = TokenKind::Define;
                            }
                            _ => {}
                        }
                        tokens.push(id);
                    }
                    '=' => {
                        tokens.push(Token {
                            kind: TokenKind::Assign,
                            value: c.to_string(),
                            loc: self.current_loc.clone(),
                        });
                        self.next();
                    }
                    _ => {}
                }
            }
        }
        Ok(tokens)
    }

    pub fn read_number(&mut self) -> Result<Token> {
        let mut num = String::new();
        let loc = self.current_loc.clone();
        let mut seen_dot = false;
        let mut seen_e = false;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                num.push(self.next().unwrap());
            } else if c == '.' {
                if seen_dot {
                    return Err(format!("Unexpected dot at: {}", self.current_loc.column));
                }
                seen_dot = true;
                num.push(self.next().unwrap());
            } else if c == 'e' {
                let loc = self.current_loc.column;
                if seen_e {
                    return Err(format!("Unexpected e at: {}", loc));
                }
                seen_e = true;
                num.push(self.next().unwrap());
                if let Some(c) = self.peek() {
                    if c == '+' || c == '-' {
                        num.push(self.next().unwrap());
                    }
                } else {
                    return Err(format!("Unexpected e at: {}", loc));
                }
            } else {
                if self.pos + 3 <= self.input.len() {
                    match self.input[self.pos..self.pos + 3] {
                        ['r', 'a', 'd'] => {
                            self.pos += 3;
                            return Ok(Token {
                                kind: TokenKind::Num(Unit::Rad),
                                value: num,
                                loc,
                            });
                        }
                        ['d', 'e', 'g'] => {
                            self.pos += 3;
                            return Ok(Token {
                                kind: TokenKind::Num(Unit::Deg),
                                value: num,
                                loc,
                            });
                        }
                        _ => {
                            break;
                        }
                    }
                } else {
                    if c.is_ascii_alphabetic() {
                        return Err(format!("Unexpected character: {}", c));
                    }
                    break;
                }
            }
        }
        Ok(Token {
            kind: TokenKind::Num(Unit::None),
            value: num,
            loc,
        })
    }

    pub fn read_id(&mut self) -> Token {
        let loc = self.current_loc.clone();
        let mut identifier = String::from(self.next().unwrap());
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                identifier.push(self.next().unwrap());
            } else {
                break;
            }
        }
        Token {
            kind: TokenKind::Id,
            value: identifier,
            loc,
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }
}

pub fn lex(input: String) -> Result<Vec<Token>> {
    let mut lexer = Lexer::new(input);
    lexer.lex()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tok(kind: TokenKind, value: &str, loc: Location) -> Token {
        Token {
            kind,
            value: String::from(value),
            loc,
        }
    }
    #[test]
    fn test_lexer() {
        let mut lexer = Lexer::new(String::from("1 + 2"));
        let tokens = lexer.lex().unwrap();
        assert_eq!(
            tokens,
            vec![
                tok(
                    TokenKind::Num(Unit::None),
                    "1",
                    Location { line: 1, column: 1 }
                ),
                tok(TokenKind::Op, "+", Location { line: 1, column: 3 }),
                tok(
                    TokenKind::Num(Unit::None),
                    "2",
                    Location { line: 1, column: 5 }
                ),
            ]
        );

        let mut lexer = Lexer::new(String::from("1 + sin(2)"));
        let tokens = lexer.lex().unwrap();
        assert_eq!(
            tokens,
            vec![
                tok(
                    TokenKind::Num(Unit::None),
                    "1",
                    Location { line: 1, column: 1 }
                ),
                tok(TokenKind::Op, "+", Location { line: 1, column: 3 }),
                tok(TokenKind::Id, "sin", Location { line: 1, column: 5 }),
                tok(TokenKind::Open, "(", Location { line: 1, column: 8 }),
                tok(
                    TokenKind::Num(Unit::None),
                    "2",
                    Location { line: 1, column: 9 }
                ),
                tok(
                    TokenKind::Close,
                    ")",
                    Location {
                        line: 1,
                        column: 10
                    }
                ),
            ]
        );

        let mut lexer = Lexer::new(String::from("1 + sin(2.1e+2) * 3.14"));
        let tokens = lexer.lex().unwrap();
        assert_eq!(
            tokens,
            vec![
                tok(
                    TokenKind::Num(Unit::None),
                    "1",
                    Location { line: 1, column: 1 }
                ),
                tok(TokenKind::Op, "+", Location { line: 1, column: 3 }),
                tok(TokenKind::Id, "sin", Location { line: 1, column: 5 }),
                tok(TokenKind::Open, "(", Location { line: 1, column: 8 }),
                tok(
                    TokenKind::Num(Unit::None),
                    "2.1e+2",
                    Location { line: 1, column: 9 }
                ),
                tok(
                    TokenKind::Close,
                    ")",
                    Location {
                        line: 1,
                        column: 15
                    }
                ),
                tok(
                    TokenKind::Op,
                    "*",
                    Location {
                        line: 1,
                        column: 17
                    }
                ),
                tok(
                    TokenKind::Num(Unit::None),
                    "3.14",
                    Location {
                        line: 1,
                        column: 19
                    }
                )
            ]
        );
    }
}
