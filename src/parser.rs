use token::Token;
use tokenizer::Tokenizer;

#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Statement {
        children: Vec<Box<ASTNode>>
    },
    Function {
        name: Option<String>,
        parameters: Vec<Token>,
        body: Box<ASTNode>,
    },
    StringLiteral {
        value: Token,
    },
    Term {
        token: Token,
    },
}

pub struct Parser {
    lexer: Tokenizer,
}

impl Parser {
    pub fn new(lexer: Tokenizer) -> Parser {
        Parser {
            lexer: lexer,
        }
    }

    pub fn parse() -> Result<ASTNode, ()> {
        Err(())
    }
}

