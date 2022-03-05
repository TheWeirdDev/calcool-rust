mod calc;
mod expr;
mod lex;
mod parse;
mod value;

use std::io::{stdin, stdout, Write};

use colored::Colorize;
use expr::Result;

use crate::{
    expr::ErrorType,
    lex::{Location, Token, TokenKind},
};

// ✓ TODO: Better error handling and messages
// ✓ TODO: More boolean operations
// TODO: If expression

fn main() -> Result<()> {
    repl()
}

fn repl() -> Result<()> {
    let mut input = String::new();
    let mut c = calc::Calculator::new();
    loop {
        print!("{}", "> ".blue());
        if let Err(e) = stdout().flush() {
            return Err((
                format!("{}", e),
                ErrorType::Static(Token {
                    kind: TokenKind::Nil,
                    value: String::new(),
                    loc: Location { line: 0, column: 0 },
                }),
            ));
        }
        match stdin().read_line(&mut input) {
            Err(e) => {
                return Err((
                    format!("{}", e),
                    ErrorType::Static(Token {
                        kind: TokenKind::Nil,
                        value: String::new(),
                        loc: Location { line: 0, column: 0 },
                    }),
                ))
            }
            Ok(0) => {
                println!("exit");
                return Ok(());
            }
            _ => {}
        }
        if input.trim().is_empty() {
            continue;
        }
        if input.trim() == "exit" {
            return Ok(());
        }

        match c.calculate(&input) {
            Ok(n) => println!("{}", n),
            Err(e) => {
                println!(
                    "{}{}",
                    match e.1 {
                        ErrorType::Static(t) => format!("{}↑\n", "_".repeat(t.loc.column + 1)),
                        _ => String::new(),
                    }
                    .green(),
                    e.0.red(),
                );
            }
        }
        input.clear();
    }
}

#[cfg(test)]
mod tests {}
