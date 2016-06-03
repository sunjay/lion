use token::Token;
use tokenizer::Tokenizer;

type Program = Vec<Statement>;

#[derive(Debug)]
pub enum Statement {
    NamedFunction {
        name: String,
        definition: Function,
    },
    AnonymousFunction(Function),
    Assignment {
        name: String,
        expr: Expr,
    },
    Expression(Expr),
}

pub type Expr = Vec<ExprItem>;

#[derive(Debug)]
pub enum ExprItem {
    AnonymousFunction(Function),
    SingleTerm(Term),
    Group(Expr),
}

#[derive(Debug)]
pub struct Function {
    params: Vec<String>,
    body: Expr,
}

#[derive(Debug)]
pub enum Term {
    Symbol(String),
    Number(f64),
    StringLiteral(String),
}

pub struct Parser {
    lexer: Tokenizer,
}

pub enum ParseError {
    SyntaxError,
}

impl Parser {
    pub fn new(lexer: Tokenizer) -> Parser {
        Parser {
            lexer: lexer,
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        Err(ParseError::SyntaxError)
    }
}

