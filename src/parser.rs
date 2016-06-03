use token::Token;
use tokenizer::{Tokenizer, LexerError};

pub type Program = Vec<Statement>;

#[derive(PartialEq, Debug)]
pub enum Statement {
    NamedFunction {
        name: String,
        definition: Function,
    },
    AnonymousFunction(Function),
    Assignment {
        name: String,
        value: Expr,
    },
    Expression(Expr),
}

pub type Expr = Vec<ExprItem>;

#[derive(PartialEq, Debug)]
pub enum ExprItem {
    AnonymousFunction(Function),
    SingleTerm(Term),
    Group(Expr),
}

#[derive(PartialEq, Debug)]
pub struct Function {
    params: Vec<String>,
    body: Expr,
}

#[derive(PartialEq, Debug)]
pub enum Term {
    Symbol(String),
    Number(f64),
    StringLiteral(String),
}

pub struct Parser {
    lexer: Tokenizer,
}

// Private type just for internal functions since the public interface
// cannot return option
type ParseResult<T> = Result<Option<T>, ParseError>;

#[derive(PartialEq, Debug)]
pub enum ParseError {
    ExpectedToken(Token),
    SyntaxError(LexerError),
}

impl Parser {
    pub fn new(lexer: Tokenizer) -> Parser {
        Parser {
            lexer: lexer,
        }
    }

    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut statements: Program = Vec::new();

        loop {
            let statement = try!(self.statement());
            if statement.is_none() {
                break;
            }

            let statement = statement.unwrap();
            statements.push(statement);
        }

        Ok(statements)
    }

    fn statement(&mut self) -> ParseResult<Statement> {
        let line = try!(self.line());
        if line.is_none() {
            return Ok(None);
        }

        let line = line.unwrap();
        self.dispatch_statement(line)
    }

    fn line(&mut self) -> ParseResult<Vec<Token>> {
        let mut line_tokens: Vec<Token> = Vec::new();

        let mut reached_eof = true;
        for next_token in &mut self.lexer {
            let next_token = try!(
                next_token.map_err(|e| ParseError::SyntaxError(e))
            );

            if next_token == Token::EOL {
                reached_eof = false;
                break;
            }

            line_tokens.push(next_token);
        }

        if reached_eof && line_tokens.is_empty() {
            return Ok(None);
        }

        Ok(Some(line_tokens))
    }

    fn dispatch_statement(&mut self, statement_tokens: Vec<Token>) -> ParseResult<Statement> {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use scanner::Scanner;
    use tokenizer::Tokenizer;

    #[test]
    fn complete_program() {
        let mut parser = parser_for(r#"
        defineUnit "km/h"

        my_var = 30 km/h
        f x yy = x km/h * 2 + (my_var / -3.5) * (8 ** yy) - 1.3e-3
        print (f 30)

        operator INFIX 6 "$$" (\x y = x * y + x)
        "#);

        let syntax_tree: Program = vec![
            Statement::Expression(vec![
                ExprItem::SingleTerm(
                    Term::Symbol("defineUnit".to_owned())
                ),
                ExprItem::SingleTerm(
                    Term::StringLiteral("km/h".to_owned())
                ),
            ]),
            Statement::Assignment {
                name: "my_var".to_owned(),
                value: vec![
                    ExprItem::SingleTerm(Term::Number(30f64)),
                    ExprItem::SingleTerm(Term::Symbol("km/h".to_owned())),
                ],
            },
            Statement::NamedFunction {
                name: "f".to_owned(),
                definition: Function {
                    params: vec![
                        "x".to_owned(),
                        "yy".to_owned()
                    ],
                    body: vec![
                        ExprItem::SingleTerm(
                            Term::Symbol("x".to_owned())
                        ),
                        ExprItem::SingleTerm(
                            Term::Symbol("km/h".to_owned())
                        ),
                        ExprItem::SingleTerm(
                            Term::Symbol("*".to_owned())
                        ),
                        ExprItem::SingleTerm(
                            Term::Number(2f64)
                        ),
                        ExprItem::SingleTerm(
                            Term::Symbol("+".to_owned())
                        ),
                        ExprItem::Group(vec![
                            ExprItem::SingleTerm(
                                Term::Symbol("my_var".to_owned())
                            ),
                            ExprItem::SingleTerm(
                                Term::Symbol("/".to_owned())
                            ),
                            ExprItem::SingleTerm(
                                Term::Number(-3.5f64)
                            ),
                        ]),
                        ExprItem::SingleTerm(
                            Term::Symbol("*".to_owned())
                        ),
                        ExprItem::Group(vec![
                            ExprItem::SingleTerm(
                                Term::Number(8f64)
                            ),
                            ExprItem::SingleTerm(
                                Term::Symbol("**".to_owned())
                            ),
                            ExprItem::SingleTerm(
                                Term::Symbol("yy".to_owned())
                            ),
                        ]),
                        ExprItem::SingleTerm(
                            Term::Symbol("-".to_owned())
                        ),
                        ExprItem::SingleTerm(
                            Term::Number(1.3e-3f64)
                        ),
                    ],
                },
            },
            Statement::Expression(vec![
                ExprItem::SingleTerm(
                    Term::Symbol("print".to_owned())
                ),
                ExprItem::Group(vec![
                    ExprItem::SingleTerm(
                        Term::Symbol("f".to_owned())
                    ),
                    ExprItem::SingleTerm(
                        Term::Number(30f64)
                    ),
                ]),
            ]),
            Statement::Expression(vec![
                ExprItem::SingleTerm(
                    Term::Symbol("operator".to_owned())
                ),
                ExprItem::SingleTerm(
                    Term::Symbol("INFIX".to_owned())
                ),
                ExprItem::SingleTerm(
                    Term::Number(6f64)
                ),
                ExprItem::Group(vec![
                    ExprItem::SingleTerm(
                        Term::StringLiteral("$$".to_owned())
                    ),
                    ExprItem::Group(vec![
                        ExprItem::AnonymousFunction(
                            Function {
                                params: vec![
                                    "x".to_owned(),
                                    "y".to_owned()
                                ],
                                body: vec![
                                    ExprItem::SingleTerm(
                                        Term::Symbol("x".to_owned())
                                    ),
                                    ExprItem::SingleTerm(
                                        Term::Symbol("*".to_owned())
                                    ),
                                    ExprItem::SingleTerm(
                                        Term::Symbol("y".to_owned())
                                    ),
                                    ExprItem::SingleTerm(
                                        Term::Symbol("+".to_owned())
                                    ),
                                    ExprItem::SingleTerm(
                                        Term::Symbol("x".to_owned())
                                    ),
                                ],
                            },
                        ),
                    ]),
                ]),
            ]),
        ];

        let parsed = parser.parse().unwrap();
        assert_eq!(parsed, syntax_tree);
    }

    fn parser_for(string: &str) -> Parser {
        Parser::new(Tokenizer::new(Scanner::from_str(string)))
    }
}

