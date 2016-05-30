#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Symbol(String),
    Backslash,
    Equals,
    ParenOpen,
    ParenClose,
    EOL,
}

