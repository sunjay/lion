extern crate lion;

use lion::parser::parse_program;

fn main() {
    let input = include_str!("../../examples/units.lion");
    println!("{:#?}", parse_program(input).unwrap());
}
