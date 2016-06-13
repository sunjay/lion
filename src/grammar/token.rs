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
    Semicolon,
}

impl Token {
    pub fn unwrap_symbol(self) -> String {
        match self {
            Token::Symbol(x) => x,
            _ => panic!("Expected to unwrap a Symbol, got {:?}", self),
        }
    }
}

