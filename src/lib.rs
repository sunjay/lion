#![cfg_attr(test, allow(dead_code))]

#[macro_use]
extern crate lazy_static;

mod scanner;
mod token;
mod tokenizer;
mod ast;
mod parser;

//TODO: Make this not public
pub use parser::Parser;

