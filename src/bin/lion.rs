extern crate lion;
extern crate nom;

use nom::{Err::Error, Context::Code};
use lion::parser::{parse_program, Span};
use lion::interpreter::Interpreter;

fn main() {
    let input = include_str!("../../examples/units.lion");
    let program = parse_program(input).unwrap_or_else(|err| match err {
        Error(Code(Span { line, .. }, _)) =>
            panic!("Syntax Error: Line {}: {}", line, input.lines().nth(line as usize).unwrap()),
        _ => unreachable!(),
    });

    let mut interpreter = Interpreter::default();
    interpreter.load_decls(&program).unwrap();
}
