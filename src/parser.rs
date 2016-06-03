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

pub type ParseResult<T> = Result<T, ParseError>;

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

    pub fn parse(&mut self) -> ParseResult<Program> {
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

    fn statement(&mut self) -> ParseResult<Option<Statement>> {
        loop {
            let line = try!(self.line());
            if line.is_none() {
                return Ok(None);
            }

            let line = line.unwrap();
            if line.is_empty() {
                continue;
            }

            return Ok(Some(try!(self.dispatch_statement(line))));
        }
    }

    fn line(&mut self) -> ParseResult<Option<Vec<Token>>> {
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

    fn dispatch_statement(&self, statement_tokens: Vec<Token>) -> ParseResult<Statement> {
        assert!(statement_tokens.len() > 0, "Got zero tokens to dispatch");
        // looking for either assignment or a named function
        // both are a collection of only symbols followed by equals
        let mut equals = None;
        for (i, token) in statement_tokens.iter().enumerate() {
            if *token == Token::Equals && i > 0 {
                equals = Some(i);
                break;
            }
            else if let Token::Symbol(_) = *token {
                // Symbols are good, continue
                continue;
            }
            else if *token == Token::Backslash && i == 0 {
                // Anonymous functions have backslashes,
                // but only in the first position
                continue;
            }
            else {
                // Found something that is not a symbol, cannot be
                // assignment or named function, stop
                break;
            }
        }

        if equals.is_none() {
            Ok(Statement::Expression(try!(self.expr(&statement_tokens[..]))))
        }
        else {
            let equals = equals.unwrap();

            // The 1 or many assumption being assumed in this
            // match is only okay because of the i > 0 above
            debug_assert!(equals > 0);

            let lhs = &statement_tokens[..equals];
            let rhs = &statement_tokens[(equals+1)..];

            if lhs[0] == Token::Backslash {
                return self.anonymous_function(lhs, rhs);
            }

            match equals {
                // only one argument: name = ...
                1 => self.assignment(lhs, rhs),
                // more than one argument: name arg1 ... = ...
                _ => self.named_function(lhs, rhs),
            }
        }
    }

    fn anonymous_function(&self, lhs: &[Token], rhs: &[Token]) -> ParseResult<Statement> {
        debug_assert!(lhs[0] == Token::Backslash);

        Ok(Statement::AnonymousFunction(try!(self.function(&lhs[0..], rhs))))
    }

    fn named_function(&self, lhs: &[Token], rhs: &[Token]) -> ParseResult<Statement> {
        let name = lhs[0].clone().unwrap_symbol();

        Ok(Statement::NamedFunction {
            name: name.clone(),
            definition: try!(self.function(&lhs[0..], rhs)),
        })
    }

    fn function(&self, params: &[Token], rhs: &[Token]) -> ParseResult<Function> {
        debug_assert!(params.len() > 0);

        Ok(Function {
            params: params.into_iter().map(|ref sym| (*sym).clone().unwrap_symbol()).collect(),
            body: try!(self.expr(rhs)),
        })
    }

    fn assignment(&self, lhs: &[Token], rhs: &[Token]) -> ParseResult<Statement> {
        debug_assert!(lhs.len() == 1);

        Ok(Statement::Assignment {
            name: lhs[0].clone().unwrap_symbol(),
            value: try!(self.expr(rhs)),
        })
    }

    fn expr(&self, tokens: &[Token]) -> ParseResult<Expr> {
        Ok(Vec::new())
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

        \q r = q / (r - 3)
        doubleMe = \e = e * 2
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
            Statement::AnonymousFunction(Function {
                params: vec!["q".to_owned(), "r".to_owned()],
                body: vec![
                    ExprItem::SingleTerm(
                        Term::Symbol("q".to_owned())
                    ),
                    ExprItem::SingleTerm(
                        Term::Symbol("/".to_owned())
                    ),
                    ExprItem::Group(vec![
                        ExprItem::SingleTerm(
                            Term::Symbol("r".to_owned())
                        ),
                        ExprItem::SingleTerm(
                            Term::Symbol("-".to_owned())
                        ),
                        ExprItem::SingleTerm(
                            Term::Number(3f64)
                        ),
                    ]),
                ],
            }),
            Statement::Assignment {
                name: "doubleMe".to_owned(),
                value: vec![
                    ExprItem::AnonymousFunction(Function {
                        params: vec!["e".to_owned()],
                        body: vec![
                            ExprItem::SingleTerm(
                                Term::Symbol("e".to_owned())
                            ),
                            ExprItem::SingleTerm(
                                Term::Symbol("*".to_owned())
                            ),
                            ExprItem::SingleTerm(
                                Term::Number(2f64)
                            ),
                        ],
                    }),
                ],
            },
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

