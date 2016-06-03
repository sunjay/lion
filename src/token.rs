#[derive(Debug, PartialEq, Clone)]
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

