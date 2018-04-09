use ast::Program;

pub type Span<'a> = &'a str;

pub type ParseResult<T> = Result<T, String>;

pub fn parse_program(input: &str) -> ParseResult<Program> {
    unimplemented!();
}
