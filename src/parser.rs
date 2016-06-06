use token::Token;
use tokenizer::{Tokenizer, LexerError};
use ast::*;

pub struct Parser {
    lexer: Tokenizer,
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(PartialEq, Debug)]
pub enum ParseError {
    ExpectedToken(Token),
    UnexpectedToken(Token),
    InvalidStringLiteral,
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
            else if line[0] == Token::Semicolon {
                if line.len() >= 2 && line[1] == Token::Symbol("-".to_owned()) {
                    continue;
                }
                else {
                    return Err(ParseError::UnexpectedToken(Token::Semicolon));
                }
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

        let function_parts = self.partition_function(&statement_tokens);

        // Error here is okay and does not need to be propagated yet because
        // it probably just means that this is an expression
        if function_parts.is_err() {
            Ok(Statement::Expression(
                try!(self.expr(&statement_tokens))
            ))
        }
        else {
            let (lhs, rhs) = function_parts.unwrap();

            // partition_function is guarenteed to return non-empty slices
            debug_assert!(!lhs.is_empty() && !rhs.is_empty());

            if lhs[0] == Token::Backslash {
                return self.anonymous_function(lhs, rhs);
            }

            match lhs.len() {
                // only one argument: name = ...
                1 => self.assignment(lhs, rhs),
                // more than one argument: name arg1 ... = ...
                _ => self.named_function(lhs, rhs),
            }
        }
    }

    fn anonymous_function(&self, lhs: &[Token], rhs: &[Token]) -> ParseResult<Statement> {
        debug_assert!(lhs[0] == Token::Backslash);

        Ok(Statement::AnonymousFunction(try!(self.function(&lhs[1..], rhs))))
    }

    fn named_function(&self, lhs: &[Token], rhs: &[Token]) -> ParseResult<Statement> {
        let name = lhs[0].clone().unwrap_symbol();

        Ok(Statement::NamedFunction {
            name: name.clone(),
            definition: try!(self.function(&lhs[1..], rhs)),
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
        let mut result = Expr::new();

        let mut remaining = tokens;
        loop {
            let (item, leftover) = try!(self.expr_item(remaining));
            remaining = leftover;

            result.push(item);

            if remaining.is_empty() {
                break;
            }
        }

        Ok(result)
    }

    fn expr_item<'a>(&self, tokens: &'a [Token]) -> ParseResult<(ExprItem, &'a [Token])> {
        debug_assert!(!tokens.is_empty());

        if tokens[0] == Token::Backslash {
            let (lhs, rhs) = try!(self.partition_function(tokens));

            debug_assert!(lhs[0] == Token::Backslash);

            Ok((
                ExprItem::AnonymousFunction(
                    try!(self.function(&lhs[1..], rhs))
                ),
                &[]
            ))
        }
        else if tokens[0] == Token::ParenOpen {
            // Need the +1 here so that we skip the opening parenthesis properly
            let close = try!(self.find_matching_parens(&tokens[1..])) + 1;

            Ok((
                ExprItem::Group(
                    try!(self.expr(&tokens[1..close]))
                ),
                &tokens[(close+1)..]
            ))
        }
        else if tokens[0] == Token::StringBoundary {
            // since strings can only be a single word, not a number
            const STRING_VALUE: usize = 1;
            const STRING_END: usize = 2;
            const AFTER_STRING: usize = 3;

            if tokens[STRING_END] != Token::StringBoundary {
                Err(ParseError::ExpectedToken(Token::StringBoundary))
            }
            else if let Token::Symbol(ref value) = tokens[STRING_VALUE] {
                Ok((ExprItem::SingleTerm(
                    Term::StringLiteral(value.clone())
                ), &tokens[AFTER_STRING..]))
            }
            else {
                Err(ParseError::InvalidStringLiteral)
            }
        }
        else {
            Ok((ExprItem::SingleTerm(
                try!(self.simple_term(&tokens[0]))
            ), &tokens[1..]))
        }
    }

    fn simple_term(&self, token: &Token) -> ParseResult<Term> {
        match *token {
            Token::Symbol(ref s) => Ok(Term::Symbol(s.clone())),
            Token::Number(ref n) => Ok(Term::Number(n.clone())),
            _ => Err(ParseError::UnexpectedToken(token.clone())),
        }
    }

    // tokens should be the tokens AFTER the first opening parenthesis
    fn find_matching_parens(&self, tokens: &[Token]) -> ParseResult<usize> {
        let mut still_open = 1;
        for (i, token) in tokens.iter().enumerate() {
            if *token == Token::ParenOpen {
                still_open += 1;
            }
            else if *token == Token::ParenClose {
                still_open -= 1;

                if still_open == 0 {
                    return Ok(i);
                }
            }
        }

        Err(ParseError::ExpectedToken(Token::ParenClose))
    }

    // partitions a function by its Equals token, returning None if no
    // valid function is found
    // Returns (lhs, rhs)
    fn partition_function<'a>(&self, tokens: &'a [Token]) -> ParseResult<(&'a [Token], &'a [Token])> {
        // looking for either assignment or a named function
        // both are a collection of only symbols followed by equals
        let mut equals = None;
        for (i, token) in tokens.iter().enumerate() {
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
                return Err(ParseError::UnexpectedToken(token.clone()));
            }
        }

        if equals.is_none() {
            Err(ParseError::ExpectedToken(Token::Equals))
        }
        else {
            let equals = equals.unwrap();

            // The 1 or many assumption being assumed in this
            // is only okay because of the i > 0 above
            debug_assert!(equals > 0);

            Ok((&tokens[..equals], &tokens[(equals+1)..]))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;
    use token::Token;
    use scanner::Scanner;
    use tokenizer::Tokenizer;

    #[test]
    fn call_function_with_string() {
        test_statement(r#"defineUnit "km/h""#,
            Statement::Expression(vec![
                ExprItem::SingleTerm(
                    Term::Symbol("defineUnit".to_owned())
                ),
                ExprItem::SingleTerm(
                    Term::StringLiteral("km/h".to_owned())
                ),
            ])
        );
    }

    #[test]
    fn define_simple_variable() {
        test_statement(r"my_var = 30 km/h",
            Statement::Assignment {
                name: "my_var".to_owned(),
                value: vec![
                    ExprItem::SingleTerm(Term::Number(30f64)),
                    ExprItem::SingleTerm(Term::Symbol("km/h".to_owned())),
                ],
            },
        );
    }

    #[test]
    fn nested_parenthesis_matching() {
        test_statement(
            r"(((x))) + (2 - (3 + (4 * 3))) % ((((75.4) - ((3 / 1e2)) + 77) * 4) / 2)",
            Statement::Expression(vec![
                ExprItem::Group(vec![
                    ExprItem::Group(vec![
                        ExprItem::Group(vec![
                            ExprItem::SingleTerm(Term::Symbol("x".to_owned())),
                        ]),
                    ]),
                ]),

                ExprItem::SingleTerm(Term::Symbol("+".to_owned())),

                ExprItem::Group(vec![
                    ExprItem::SingleTerm(Term::Number(2f64)),
                    ExprItem::SingleTerm(Term::Symbol("-".to_owned())),
                    ExprItem::Group(vec![
                        ExprItem::SingleTerm(Term::Number(3f64)),
                        ExprItem::SingleTerm(Term::Symbol("+".to_owned())),
                        ExprItem::Group(vec![
                            ExprItem::SingleTerm(Term::Number(4f64)),
                            ExprItem::SingleTerm(Term::Symbol("*".to_owned())),
                            ExprItem::SingleTerm(Term::Number(3f64)),
                        ]),
                    ]),
                ]),

                ExprItem::SingleTerm(Term::Symbol("%".to_owned())),

                ExprItem::Group(vec![
                    ExprItem::Group(vec![
                        ExprItem::Group(vec![
                            ExprItem::Group(vec![
                                ExprItem::SingleTerm(Term::Number(75.4f64)),
                            ]),
                            ExprItem::SingleTerm(Term::Symbol("-".to_owned())),
                            ExprItem::Group(vec![
                                ExprItem::Group(vec![
                                    ExprItem::SingleTerm(Term::Number(3f64)),
                                    ExprItem::SingleTerm(Term::Symbol("/".to_owned())),
                                    ExprItem::SingleTerm(Term::Number(1e2f64)),
                                ]),
                            ]),
                            ExprItem::SingleTerm(Term::Symbol("+".to_owned())),
                            ExprItem::SingleTerm(Term::Number(77f64)),
                        ]),
                        ExprItem::SingleTerm(Term::Symbol("*".to_owned())),
                        ExprItem::SingleTerm(Term::Number(4f64)),
                    ]),
                    ExprItem::SingleTerm(Term::Symbol("/".to_owned())),
                    ExprItem::SingleTerm(Term::Number(2f64)),
                ]),
            ])
        );
    }

    #[test]
    fn named_function_complex_expression() {
        test_statement(r"f x yy = x km/h * 2 + (my_var / -3.5) * (8 ** yy) - 1.3e-3",
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
        );
    }

    #[test]
    fn ignores_comments() {
        let program = r#"
        ;- some comment
        x
            ;- Another comment but indented
        1
        4
        ;- ;- ;- ;- doesn't matter what else is on the line
        2
        ;- sometimes the comment prefix is just on its own line:
        ;-
        ;- all characters should be allowed ~!@# $%^&*()_+"':';;;[]{}\|?></.,
        "#;

        let syntax_tree: Program = vec![
            Statement::Expression(vec![
                ExprItem::SingleTerm(
                    Term::Symbol("x".to_owned())
                ),
            ]),
            Statement::Expression(vec![
                ExprItem::SingleTerm(
                    Term::Symbol("1".to_owned())
                ),
            ]),
            Statement::Expression(vec![
                ExprItem::SingleTerm(
                    Term::Symbol("4".to_owned())
                ),
            ]),
            Statement::Expression(vec![
                ExprItem::SingleTerm(
                    Term::Symbol("2".to_owned())
                ),
            ]),
        ];

        test_program(program, syntax_tree);
    }

    #[test]
    fn rejects_invalid_comment_beginnings() {
        test_invalid_statement(";-something", ParseError::UnexpectedToken(Token::Semicolon));
    }

    #[test]
    fn rejects_comment_elsewhere_on_line() {
        test_invalid_statement("valid ;- something", ParseError::UnexpectedToken(Token::Semicolon));
    }

    #[test]
    fn rejects_lone_comment_semicolon() {
        test_invalid_statement(";", ParseError::UnexpectedToken(Token::Semicolon));
    }

    //TODO: Tests for failure cases like:
    //TODO: * mismatched parenthesis both too many `(` and too many `)`
    //TODO: * mismatched quotes on strings
    //TODO: * multi-word strings
    //TODO: * named functions within other statement types

    #[test]
    fn complete_program() {
        let program = r#"
        defineUnit "km/h"

        my_var = 30 km/h
        f x yy = x km/h * 2 + (my_var / -3.5) * (8 ** yy) - 1.3e-3
        print (f 30)

        \q r = q / (r - 3)
        doubleMe = \e = e * 2
        operator INFIX 6 "$$" (\x y = x * y + x)
        "#;

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
        ];

        test_program(program, syntax_tree);
    }

    fn test_program(string: &str, expected: Program) {
        let mut parser = parser_for(string);
        let parsed = parser.parse().unwrap();
        assert_eq!(parsed, expected);
    }

    fn test_statement(string: &str, expected: Statement) {
        let mut parser = parser_for(string);
        let parsed = parser.parse().unwrap();
        assert_eq!(parsed[0], expected);
    }

    fn test_invalid_statement(string: &str, expected_error: ParseError) {
        let mut parser = parser_for(string);
        let error = parser.parse().unwrap_err();
        assert_eq!(error, expected_error);
    }

    fn parser_for(string: &str) -> Parser {
        Parser::new(Tokenizer::new(Scanner::from_str(string)))
    }
}

