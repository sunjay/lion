use ast::Program;
use parser::parser::{Parser, ParseResult};
use parser::tokenizer::Tokenizer;
use parser::scanner::Scanner;

pub fn parse(string: &str) -> ParseResult<Program> {
    Parser::new(
        Tokenizer::new(
            Scanner::from_str(string)
        )
    ).parse()
}

