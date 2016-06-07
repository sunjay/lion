#[macro_use]
extern crate lazy_static;

pub mod ast;

mod scanner;
mod token;
mod tokenizer;
mod parser;
mod rich_number;
mod prelude;
mod eval_context;

mod api;

pub use api::*;
