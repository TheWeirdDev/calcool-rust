use std::collections::HashMap;

use crate::{
    expr::{Expr, Result},
    lex::lex,
    parse,
};

#[derive(Clone, Debug)]
pub enum MemoryItem {
    Num(f64),
    Func(Vec<String>, Box<Expr>),
}
pub type Memory = HashMap<String, MemoryItem>;

#[derive(Debug, Clone)]
pub struct Calculator {
    pub memory: Memory,
}

impl Calculator {
    pub fn new() -> Self {
        Calculator {
            memory: HashMap::new(),
        }
    }
    pub fn calculate(&mut self, input: &String) -> Result<f64> {
        let tokens = lex(input.clone())?;
        let p = parse::Parser::new(tokens).parse();
        p?.eval_with_memory(&mut self.memory)
    }
}
