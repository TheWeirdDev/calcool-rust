use std::collections::HashMap;

use crate::{expr::Result, lex::lex, parse, value::Value};

pub type Memory = HashMap<String, Value>;

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
    pub fn calculate(&mut self, input: &String) -> Result<Value> {
        let tokens = lex(input.clone())?;
        let p = parse::Parser::new(tokens).parse();
        p?.eval_with_memory(&mut self.memory)
    }
}
