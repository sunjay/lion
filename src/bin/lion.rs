extern crate lion;

use lion::parser::parse_program;

fn main() {
    let input = include_str!("../../examples/units.lion");
    parse_program(input).unwrap_or_else(|e| panic!("{}", e));
}
