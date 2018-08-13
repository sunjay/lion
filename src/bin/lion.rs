extern crate lion;
extern crate nom;

use nom::{Err::Error, Context::Code};
use lion::parser::{parse_program, Span};
use lion::context::Context;

fn main() {
    let input = include_str!("../../examples/units.lion");
    let program = match parse_program(input) {
        Ok(program) => program,
        Err(Error(Code(Span { line, .. }, _))) =>
            panic!("Syntax Error: Line {}: {}", line, input.lines().nth(line as usize).unwrap()),
        Err(_) => unreachable!(),
    };

    let mut context = Context::default();
    context.walk_decls(&program);
}
