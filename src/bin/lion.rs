extern crate lion;

use lion::parser::parse_program;
use lion::context::Context;

fn main() {
    let input = include_str!("../../examples/units.lion");
    let program = parse_program(input).unwrap();

    let mut context = Context::default();
    context.walk_decls(&program);
}
