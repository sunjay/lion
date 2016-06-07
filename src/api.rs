use ast::Program;
use parser::{Parser, ParseResult};
use tokenizer::Tokenizer;
use scanner::Scanner;

pub fn parse(string: &str) -> ParseResult<Program> {
    Parser::new(
        Tokenizer::new(
            Scanner::from_str(string)
        )
    ).parse()
}

