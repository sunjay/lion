#[macro_use]
extern crate lazy_static;

mod scanner;
mod token;
mod tokenizer;

//TODO: Make this not public
pub use tokenizer::Tokenizer;

