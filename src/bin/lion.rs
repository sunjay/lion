extern crate lion;

use lion::ast;
use lion::parse;

fn main() {
    let expr = "2 + (2 * (3 - 4))";
    println!("Expression:\n{}\n\nAbstract Syntax Tree:\n", expr);
    println!("{:#?}", parse(expr).unwrap());
}
