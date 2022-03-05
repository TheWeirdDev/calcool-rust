use std::fmt::Display;

use crate::expr::{ErrorType, Expr, Result};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Func(Vec<String>, Box<Expr>, bool),
    Bool(bool),
    Type(String),
    Nil,
}

impl Value {
    pub fn as_num(&self) -> Result<f64> {
        match self {
            Value::Num(n) => Ok(*n),
            _ => Err((format!("{} is not a number", self), ErrorType::Dynamic)),
        }
    }

    pub fn get_type(&self) -> String {
        return format!(
            "[Type: {}]",
            match self {
                Value::Num(_) => "Number",
                Value::Func(_, _, ref builtin) =>
                    if *builtin {
                        "Builtin Function"
                    } else {
                        "Function"
                    },
                Value::Bool(_) => "Boolean",
                Value::Type(_) => "Type",
                Value::Nil => "Nil",
            }
        );
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => write!(f, "{}", n),
            Value::Func(_, _, builtin) => {
                write!(
                    f,
                    "{}",
                    if *builtin {
                        "Function (Builtin)"
                    } else {
                        "Function"
                    }
                )
            }
            Value::Bool(b) => write!(f, "{}", b),
            Value::Type(t) => write!(f, "{}", t),
            Value::Nil => write!(f, "nil"),
        }
    }
}
