extern crate lion;
extern crate readline;

use readline::{readline, add_history, Error as ReadlineError};

use lion::parse;
use lion::eval::eval_context::EvalContext;

fn main() {
    let mut context = EvalContext::prelude();

    loop {
        let line = match readline("\u{03BB} ") {
            Ok(data) => data,
            Err(ReadlineError::EndOfFile) => break,
            Err(e) => {
                println!("{}", e);
                break;
            },
        };
        add_history(&line).expect("Error adding line to history");

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

