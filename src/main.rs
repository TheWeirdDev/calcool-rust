mod calc;
mod expr;
mod lex;
mod parse;

use std::io::{stdin, stdout, Write};

use expr::Result;
fn main() -> Result<()> {
    repl()
}

fn repl() -> Result<()> {
    let mut input = String::new();
    let mut c = calc::Calculator::new();
    loop {
        print!("> ");
        if let Err(e) = stdout().flush() {
            return Err(format!("{}", e));
        }
        match stdin().read_line(&mut input) {
            Err(e) => return Err(format!("{}", e)),
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
            Err(e) => println!("Error: {}", e),
        }
        input.clear();
    }
}

#[cfg(test)]
mod tests {}
