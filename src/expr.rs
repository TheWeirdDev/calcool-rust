use crate::{
    calc::{Memory, MemoryItem},
    lex::{Token, TokenKind},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

pub type Result<T> = std::result::Result<T, String>;

#[derive(Debug, PartialEq, Eq, Clone)]

pub enum Unit {
    None,
    Rad,
    Deg,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Num(Token),
    Unary(String, Box<Expr>),
    Binary(String, Box<Expr>, Box<Expr>),
    Call(String, Vec<Box<Expr>>),
    Var(String),
    Assign(String, Box<Expr>),
    Define(String, Vec<String>, Box<Expr>),
}

impl Expr {
    #[allow(dead_code)]
    pub fn eval(&self) -> Result<f64> {
        self.eval_impl(Rc::new(RefCell::new(None)))
    }

    pub fn eval_with_memory(&self, memory: &mut Memory) -> Result<f64> {
        self.eval_impl(Rc::new(RefCell::new(Some(memory))))
    }

    fn eval_impl<'a>(&self, mem: Rc<RefCell<Option<&'a mut Memory>>>) -> Result<f64> {
        match self {
            Expr::Num(n) => {
                if let Ok(val) = n.value.parse() {
                    if let TokenKind::Num(unit) = &n.kind {
                        match unit {
                            Unit::Deg => Ok(val * std::f64::consts::PI / 180.0),
                            _ => Ok(val),
                        }
                    } else {
                        Ok(val)
                    }
                } else {
                    return Err(format!("Invalid number: {}", n.value));
                }
            }
            Expr::Unary(f, e) => Ok(match f.as_str() {
                "-" => -e.eval_impl(mem.clone())?,
                "+" => e.eval_impl(mem.clone())?,
                _ => return Err(format!("Invalid unary operator: {}", f)),
            }),
            Expr::Binary(f, e1, e2) => {
                let e1 = e1.eval_impl(mem.clone())?;
                let e2 = e2.eval_impl(mem.clone())?;
                Ok(match f.as_str() {
                    "+" => e1 + e2,
                    "-" => e1 - e2,
                    "*" => e1 * e2,
                    "/" => e1 / e2,
                    "^" => e1.powf(e2),
                    _ => return Err(format!("Invalid binary operator: {}", f)),
                })
            }
            &Expr::Call(ref f, ref e) => {
                let params = e
                    .iter()
                    .map(|e| e.eval_impl(mem.clone()))
                    .collect::<Result<Vec<_>>>()?;
                let fns: [(&str, fn(f64) -> f64); 17] = [
                    ("sqrt", |v| v.sqrt()),
                    ("abs", |v| v.abs()),
                    ("sin", |v| v.sin()),
                    ("cos", |v| v.cos()),
                    ("tan", |v| v.tan()),
                    ("asin", |v| v.asin()),
                    ("acos", |v| v.acos()),
                    ("atan", |v| v.atan()),
                    ("exp", |v| v.exp()),
                    ("ln", |v| v.ln()),
                    ("log", |v| v.log10()),
                    ("log2", |v| v.log2()),
                    ("floor", |v| v.floor()),
                    ("ceil", |v| v.ceil()),
                    ("round", |v| v.round()),
                    ("trunc", |v| v.trunc()),
                    ("signum", |v| v.signum()),
                ];
                let builtin_functions = HashMap::<&str, fn(f64) -> f64>::from(fns);
                match builtin_functions.get(f.as_str()) {
                    Some(func) => {
                        if e.len() != 1 {
                            return Err(format!("Invalid number of arguments for function: {}", f));
                        }
                        Ok(func(params[0]))
                    }
                    None => match mem.borrow().as_deref() {
                        Some(m) => {
                            if let Some(item) = m.get(f) {
                                match item {
                                    MemoryItem::Func(args, body) => {
                                        let mut new_mem = m.clone();
                                        if args.len() != params.len() {
                                            return Err(format!(
                                                "Invalid number of arguments for function: {}",
                                                f
                                            ));
                                        }
                                        for (arg, value) in args.iter().zip(params) {
                                            new_mem.insert(arg.clone(), MemoryItem::Num(value));
                                        }
                                        Ok(body
                                            .eval_impl(Rc::new(RefCell::new(Some(&mut new_mem))))?)
                                    }
                                    _ => return Err(format!("Invalid function: {}", f)),
                                }
                            } else {
                                Err(format!("Undefined function: {}", f))
                            }
                        }
                        None => return Err(format!("Invalid function: {}", f)),
                    },
                }
            }
            Expr::Var(e) => match e.to_lowercase().as_str() {
                "pi" => Ok(std::f64::consts::PI),
                "e" => Ok(std::f64::consts::E),
                _ => match mem.borrow().as_ref() {
                    Some(m) => match m.get(e) {
                        Some(v) => Ok(match v {
                            &MemoryItem::Num(n) => n,
                            _ => return Err(format!("Invalid variable: {}", e)),
                        }),
                        None => Err(format!("Unknown variable: {}", e)),
                    },
                    None => Err(format!("Unknown variable: {}", e)),
                },
            },
            Expr::Assign(var, e) => {
                if ["pi", "e"].contains(&var.to_lowercase().as_str()) {
                    return Err(format!("Cannot assign to constant: {}", var));
                }
                let e = e.eval_impl(mem.clone())?;
                match mem.borrow_mut().as_mut() {
                    Some(m) => {
                        m.insert(var.clone(), MemoryItem::Num(e));
                        Ok(e)
                    }
                    None => Err("No memory to assign to".to_string()),
                }
            }
            Expr::Define(var, args, e) => {
                if ["pi", "e"].contains(&var.to_lowercase().as_str()) {
                    return Err(format!("Cannot define constant: {}", var));
                }
                match mem.borrow_mut().as_mut() {
                    Some(m) => {
                        m.insert(var.clone(), MemoryItem::Func(args.clone(), e.clone()));
                        Ok(1.0)
                    }
                    None => Err("No memory to define to".to_string()),
                }
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expr::Num(ref n) => format!("{}", n.value),
                Expr::Unary(ref f, ref e) => format!("{}{}", f, e),
                Expr::Binary(ref f, ref e1, ref e2) => {
                    format!("{} {} {}", e1, f, e2)
                }
                Expr::Call(ref f, ref e) => format!(
                    "{}({})",
                    f,
                    format!(
                        "{}",
                        e.iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                ),
                Expr::Var(ref e) => format!("{}", e),
                Expr::Assign(ref e, ref e2) => format!("{} = {}", e, e2),
                Expr::Define(ref e, ref e2, ref e3) =>
                    format!("{}({}) = {}", e, format!("{}", e2.join(",")), e3),
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::lex::{Location, TokenKind};

    use super::*;

    fn tok(val: String, kind: TokenKind) -> Token {
        Token {
            value: val,
            kind,
            loc: Location { line: 0, column: 0 },
        }
    }

    #[test]
    fn test_eval() {
        assert_eq!(
            Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None))).eval(),
            Ok(1.0)
        );
        assert_eq!(
            Expr::Num(tok("1.0".to_string(), TokenKind::Num(Unit::None))).eval(),
            Ok(1.0)
        );
        assert_eq!(
            Expr::Num(tok("1.0e2".to_string(), TokenKind::Num(Unit::None))).eval(),
            Ok(100.0)
        );
        assert_eq!(
            Expr::Num(tok("1.0e-2".to_string(), TokenKind::Num(Unit::None))).eval(),
            Ok(0.01)
        );
        assert_eq!(
            Expr::Num(tok("1.0e+2".to_string(), TokenKind::Num(Unit::None))).eval(),
            Ok(100.0)
        );
    }

    #[test]
    fn test_unary() {
        assert_eq!(
            Expr::Unary(
                "-".to_string(),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(-1.0)
        );
    }

    #[test]
    fn test_binary() {
        assert_eq!(
            Expr::Binary(
                "+".to_string(),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(3.0)
        );
        assert_eq!(
            Expr::Binary(
                "-".to_string(),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(-1.0)
        );
        assert_eq!(
            Expr::Binary(
                "*".to_string(),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(2.0)
        );
        assert_eq!(
            Expr::Binary(
                "/".to_string(),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(0.5)
        );
    }

    #[test]
    fn test_call() {
        assert_eq!(
            Expr::Call(
                "sqrt".to_string(),
                vec![Box::new(Expr::Num(tok(
                    "4".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(2.0)
        );
        assert_eq!(
            Expr::Call(
                "abs".to_string(),
                vec![Box::new(Expr::Num(tok(
                    "-10".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(10.0)
        );
        assert_eq!(
            Expr::Call(
                "sin".to_string(),
                vec![Box::new(Expr::Num(tok(
                    "0".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(0.0)
        );
        assert_eq!(
            Expr::Call(
                "cos".to_string(),
                vec![Box::new(Expr::Num(tok(
                    "0".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(1.0)
        );
        assert_eq!(
            Expr::Call(
                "tan".to_string(),
                vec![Box::new(Expr::Num(tok(
                    "0".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(0.0)
        );
    }

    #[test]
    fn test_print() {
        assert_eq!(
            Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None))).to_string(),
            "1"
        );
        assert_eq!(
            Expr::Unary(
                "-".to_string(),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None))))
            )
            .to_string(),
            "-1"
        );
        assert_eq!(
            Expr::Binary(
                "+".to_string(),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .to_string(),
            "1 + 2"
        );
        assert_eq!(
            Expr::Call(
                "sqrt".to_string(),
                vec![Box::new(Expr::Num(tok(
                    "1".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .to_string(),
            "sqrt(1)"
        );

        assert_eq!(
            Expr::Call(
                "atan2".to_string(),
                vec![
                    Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                    Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
                ]
            )
            .to_string(),
            "atan2(1, 2)"
        );
    }
}
