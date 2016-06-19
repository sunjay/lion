extern crate lion;

use std::io;
use std::io::prelude::*;

use lion::parse;
use lion::eval::eval_context::EvalContext;

fn main() {
    let mut context = EvalContext::prelude();

    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();

    loop {
        print!("\u{03BB} ");
        io::stdout().flush().expect("Error while flushing stdout");

        let line = lines.next();
        if line.is_none() {
            break;
        }
        let line = line.unwrap();

        if line.is_err() {
            panic!("{}", line.unwrap_err());
        }
        let line = line.unwrap();

        let program = parse(&line);
        if program.is_err() {
            println!("Parse error: {:?}", program.unwrap_err());
            continue;
        }
        let program = program.unwrap();

        for stmt in program {
            let result = context.apply(stmt);

            if result.is_ok() {
                println!("{:?}", result.unwrap());
            }
            else {
                println!("Error: {:?}", result.unwrap_err());
                break;
            }
        }
    }
}

