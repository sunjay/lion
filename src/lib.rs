#[macro_use]
extern crate lazy_static;

pub mod ast;

mod scanner;
mod token;
mod tokenizer;
mod parser;

pub fn parse(string: &str) -> parser::ParseResult<ast::Program> {
    parser::Parser::new(
        tokenizer::Tokenizer::new(
            scanner::Scanner::from_str(string)
        )
    ).parse()
}

