#[derive(Debug, PartialEq)]
pub enum Token {
    Symbol(String),
    Backslash,
    Equals,
    ParenOpen,
    ParenClose,
    StringBoundary,
    Number(f64),
    EOL,
}

