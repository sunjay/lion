extern crate lion;
extern crate nom;
extern crate linefeed;

use std::io::Error as IOError;

use nom::{Err::Error, Context::Code};
use linefeed::{Interface, ReadResult};
use lion::parser::{parse_program, parse_expr, Span};
use lion::interpreter::Interpreter;

fn main() -> Result<(), IOError> {
    let input = include_str!("../../examples/units.lion");
    let program = parse_program(input).unwrap_or_else(|err| match err {
        Error(Code(Span { line, .. }, _)) =>
            panic!("Syntax Error: Line {}: {}", line, input.lines().nth(line as usize).unwrap()),
        _ => unreachable!(),
    });

    let mut interpreter = Interpreter::default();
    interpreter.load_decls(&program).unwrap();

    let reader = Interface::new("lion")?;
    reader.set_prompt("% ")?;

    while let ReadResult::Input(input) = reader.read_line()? {
        let input = input.trim();
        let expr = parse_expr(input).unwrap_or_else(|err| match err {
            Error(Code(Span { line, .. }, _)) =>
                panic!("Syntax Error: Line {}: {}", line, input.lines().nth(line as usize).unwrap()),
            _ => unreachable!(),
        });

        let result = interpreter.evaluate_expr(&expr);
        println!("{:?}", result);
    }

    Ok(())
}
