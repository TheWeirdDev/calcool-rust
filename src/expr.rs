use crate::{
    calc::Memory,
    lex::{Token, TokenKind},
    value::Value,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
    Simple,
    Static(Token),
    Dynamic, //(StackTrace),
}

pub type Result<T> = std::result::Result<T, (String, ErrorType)>;

#[derive(Debug, PartialEq, Eq, Clone)]

pub enum Unit {
    None,
    Rad,
    Deg,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(Token),
    Nil(Token),
    Bool(Token, bool),
    Unary(Token, Box<Expr>),
    Binary(Token, Box<Expr>, Box<Expr>),
    Call(Token, Vec<Box<Expr>>),
    Var(Token),
    Assign(Token, Box<Expr>),
    Define(Token, Vec<String>, Box<Expr>),
}

fn get_builtin_functions() -> HashMap<&'static str, fn(f64) -> f64> {
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
    return HashMap::from(fns);
}
impl Expr {
    #[allow(dead_code)]
    pub fn eval(&self) -> Result<Value> {
        self.eval_impl(Rc::new(RefCell::new(None)))
    }

    pub fn eval_with_memory(&self, memory: &mut Memory) -> Result<Value> {
        self.eval_impl(Rc::new(RefCell::new(Some(memory))))
    }

    fn eval_impl<'a>(&self, mem: Rc<RefCell<Option<&'a mut Memory>>>) -> Result<Value> {
        match self {
            Expr::Nil(_) => Ok(Value::Nil),
            Expr::Bool(_, b) => Ok(Value::Bool(*b)),
            Expr::Num(n) => {
                if let Ok(val) = n.value.parse::<f64>() {
                    if let TokenKind::Num(unit) = &n.kind {
                        match unit {
                            Unit::Deg => Ok(Value::Num(val * std::f64::consts::PI / 180.0)),
                            _ => Ok(Value::Num(val)),
                        }
                    } else {
                        Ok(Value::Num(val))
                    }
                } else {
                    return Err((
                        format!("Invalid number: {}", n.value),
                        ErrorType::Static(n.clone()),
                    ));
                }
            }
            Expr::Unary(t, e) => Ok(match t.value.as_str() {
                "-" => Value::Num(-e.eval_impl(mem.clone())?.as_num()?),
                "+" => e.eval_impl(mem.clone())?,
                _ => {
                    return Err((
                        format!("Invalid unary operator: {}", t.value),
                        ErrorType::Static(t.clone()),
                    ))
                }
            }),
            Expr::Binary(f, e1, e2) => {
                let e1 = e1.eval_impl(mem.clone())?.as_num()?;
                let e2 = e2.eval_impl(mem.clone())?.as_num()?;
                Ok(match f.value.as_str() {
                    "+" => Value::Num(e1 + e2),
                    "-" => Value::Num(e1 - e2),
                    "*" => Value::Num(e1 * e2),
                    "/" => Value::Num(e1 / e2),
                    "^" => Value::Num(e1.powf(e2)),
                    "<" => Value::Bool(e1 < e2),
                    ">" => Value::Bool(e1 > e2),
                    "==" => Value::Bool(e1 == e2),
                    "!=" => Value::Bool(e1 != e2),
                    "<=" => Value::Bool(e1 <= e2),
                    ">=" => Value::Bool(e1 >= e2),
                    _ => {
                        return Err((
                            format!("Invalid binary operator: {}", f.value),
                            ErrorType::Static(f.clone()),
                        ))
                    }
                })
            }
            Expr::Call(ref f, ref e) => {
                let params = e
                    .iter()
                    .map(|e| e.eval_impl(mem.clone()))
                    .collect::<Result<Vec<_>>>()?;
                let builtin_functions = get_builtin_functions();
                if f.value == "typeof" {
                    if let Some(v) = params.get(0) {
                        return Ok(Value::Type(v.get_type()));
                    } else {
                        return Err((
                            "typeof requires one argument".to_string(),
                            ErrorType::Static(f.clone()),
                        ));
                    }
                }
                if f.value == "print" {
                    if let Some(v) = params.get(0) {
                        println!("{}", v);
                        return Ok(Value::Nil);
                    } else {
                        return Err((
                            "print requires one argument".to_string(),
                            ErrorType::Static(f.clone()),
                        ));
                    }
                }
                match builtin_functions.get(f.value.as_str()) {
                    Some(func) => {
                        if e.len() != 1 {
                            return Err((
                                format!("Invalid number of arguments for function: {}", f.value),
                                ErrorType::Static(f.clone()),
                            ));
                        }
                        Ok(Value::Num(func(params[0].as_num()?)))
                    }
                    None => match mem.borrow().as_deref() {
                        Some(m) => {
                            if let Some(item) = m.get(f.value.as_str()) {
                                match item {
                                    Value::Func(args, body, _) => {
                                        let mut new_mem = m.clone();
                                        if args.len() != params.len() {
                                            return Err((
                                                format!(
                                                    "Invalid number of arguments for function: {}",
                                                    f.value
                                                ),
                                                ErrorType::Static(f.clone()),
                                            ));
                                        }
                                        for (arg, value) in args.iter().zip(params) {
                                            new_mem.insert(arg.clone(), value);
                                        }
                                        Ok(body
                                            .eval_impl(Rc::new(RefCell::new(Some(&mut new_mem))))?)
                                    }
                                    _ => {
                                        return Err((
                                            format!("Invalid function: {}", f.value),
                                            ErrorType::Static(f.clone()),
                                        ))
                                    }
                                }
                            } else {
                                Err((
                                    format!("Undefined function: {}", f.value),
                                    ErrorType::Static(f.clone()),
                                ))
                            }
                        }
                        None => {
                            return Err((
                                format!("Invalid function: {}", f.value),
                                ErrorType::Static(f.clone()),
                            ))
                        }
                    },
                }
            }
            Expr::Var(e) => match e.value.to_lowercase().as_str() {
                "pi" => Ok(Value::Num(std::f64::consts::PI)),
                "e" => Ok(Value::Num(std::f64::consts::E)),
                _ => match get_builtin_functions().get(e.value.as_str()) {
                    Some(_) => Ok(Value::Func(
                        vec!["num".to_string()],
                        Box::new(Expr::Nil(e.clone())),
                        true,
                    )),
                    None => match mem.borrow().as_ref() {
                        Some(m) => match m.get(e.value.as_str()) {
                            Some(v) => Ok(v.clone()),
                            None => Err((
                                format!("Unknown variable: {}", e.value),
                                ErrorType::Static(e.clone()),
                            )),
                        },
                        None => Err((
                            format!("Unknown variable: {}", e.value),
                            ErrorType::Static(e.clone()),
                        )),
                    },
                },
            },
            Expr::Assign(var, e) => {
                if ["pi", "e"].contains(&var.value.to_lowercase().as_str()) {
                    return Err((
                        format!("Cannot assign to constant: {}", var.value),
                        ErrorType::Static(var.clone()),
                    ));
                }
                if get_builtin_functions().contains_key(var.value.as_str()) {
                    return Err((
                        format!("Cannot assign to builtin function: {}", var.value),
                        ErrorType::Static(var.clone()),
                    ));
                }
                let e = e.eval_impl(mem.clone())?;
                match mem.borrow_mut().as_mut() {
                    Some(m) => {
                        m.insert(var.value.clone(), e.clone());
                        Ok(e)
                    }
                    None => Err((
                        "No memory to assign to".to_string(),
                        ErrorType::Static(var.clone()),
                    )),
                }
            }
            Expr::Define(var, args, e) => {
                if ["pi", "e"].contains(&var.value.to_lowercase().as_str()) {
                    return Err((
                        format!("Cannot define constant: {}", var.value),
                        ErrorType::Static(var.clone()),
                    ));
                }
                match mem.borrow_mut().as_mut() {
                    Some(m) => {
                        let value = Value::Func(args.clone(), e.clone(), false);
                        m.insert(var.value.clone(), value.clone());
                        Ok(value)
                    }
                    None => Err((
                        "No memory to define to".to_string(),
                        ErrorType::Static(var.clone()),
                    )),
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
                Expr::Nil(_) => "nil".to_string(),
                Expr::Bool(_, b) => b.to_string(),
                Expr::Num(ref n) => format!("{}", n.value),
                Expr::Unary(ref f, ref e) => format!("{}{}", f.value, e),
                Expr::Binary(ref f, ref e1, ref e2) => {
                    format!("{} {} {}", e1, f.value, e2)
                }
                Expr::Call(ref f, ref e) => format!(
                    "{}({})",
                    f.value,
                    format!(
                        "{}",
                        e.iter()
                            .map(|e| e.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                ),
                Expr::Var(ref e) => format!("{}", e.value),
                Expr::Assign(ref e, ref e2) => format!("{} = {}", e.value, e2),
                Expr::Define(ref e, ref e2, ref e3) =>
                    format!("{}({}) = {}", e.value, format!("{}", e2.join(",")), e3),
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
            Ok(Value::Num(1.0))
        );
        assert_eq!(
            Expr::Num(tok("1.0".to_string(), TokenKind::Num(Unit::None))).eval(),
            Ok(Value::Num(1.0))
        );
        assert_eq!(
            Expr::Num(tok("1.0e2".to_string(), TokenKind::Num(Unit::None))).eval(),
            Ok(Value::Num(100.0))
        );
        assert_eq!(
            Expr::Num(tok("1.0e-2".to_string(), TokenKind::Num(Unit::None))).eval(),
            Ok(Value::Num(0.01))
        );
        assert_eq!(
            Expr::Num(tok("1.0e+2".to_string(), TokenKind::Num(Unit::None))).eval(),
            Ok(Value::Num(100.0))
        );
    }

    #[test]
    fn test_unary() {
        assert_eq!(
            Expr::Unary(
                tok("-".to_string(), TokenKind::Op),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(Value::Num(-1.0))
        );
    }

    #[test]
    fn test_binary() {
        assert_eq!(
            Expr::Binary(
                tok("+".to_string(), TokenKind::Op),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(Value::Num(3.0))
        );
        assert_eq!(
            Expr::Binary(
                tok("-".to_string(), TokenKind::Op),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(Value::Num(-1.0))
        );
        assert_eq!(
            Expr::Binary(
                tok("*".to_string(), TokenKind::Op),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(Value::Num(2.0))
        );
        assert_eq!(
            Expr::Binary(
                tok("/".to_string(), TokenKind::Op),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .eval(),
            Ok(Value::Num(0.5))
        );
    }

    #[test]
    fn test_call() {
        assert_eq!(
            Expr::Call(
                tok("sqrt".to_string(), TokenKind::Id),
                vec![Box::new(Expr::Num(tok(
                    "4".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(Value::Num(2.0))
        );
        assert_eq!(
            Expr::Call(
                tok("abs".to_string(), TokenKind::Id),
                vec![Box::new(Expr::Num(tok(
                    "-10".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(Value::Num(10.0))
        );
        assert_eq!(
            Expr::Call(
                tok("sin".to_string(), TokenKind::Id),
                vec![Box::new(Expr::Num(tok(
                    "0".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(Value::Num(0.0))
        );
        assert_eq!(
            Expr::Call(
                tok("cos".to_string(), TokenKind::Id),
                vec![Box::new(Expr::Num(tok(
                    "0".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(Value::Num(1.0))
        );
        assert_eq!(
            Expr::Call(
                tok("tan".to_string(), TokenKind::Id),
                vec![Box::new(Expr::Num(tok(
                    "0".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .eval(),
            Ok(Value::Num(0.0))
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
                tok("-".to_string(), TokenKind::Op),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None))))
            )
            .to_string(),
            "-1"
        );
        assert_eq!(
            Expr::Binary(
                tok("+".to_string(), TokenKind::Op),
                Box::new(Expr::Num(tok("1".to_string(), TokenKind::Num(Unit::None)))),
                Box::new(Expr::Num(tok("2".to_string(), TokenKind::Num(Unit::None))))
            )
            .to_string(),
            "1 + 2"
        );
        assert_eq!(
            Expr::Call(
                tok("sqrt".to_string(), TokenKind::Id),
                vec![Box::new(Expr::Num(tok(
                    "1".to_string(),
                    TokenKind::Num(Unit::None)
                )))]
            )
            .to_string(),
            "sqrt(1)"
        );
    }
}
