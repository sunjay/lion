extern crate lion;

use lion::parse;

fn main() {
    println!("{:?}", parse("(2 + 2) * (3 - 4)").unwrap());
}
