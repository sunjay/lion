use std::iter::Iterator;
use std::result;
use std::collections::HashSet;

use grammar::token::Token;
use parser::scanner::Scanner;

const EOL_CHAR: char = '\n';
const BACKSLASH_CHAR: char = '\\';
const PAREN_OPEN_CHAR: char = '(';
const PAREN_CLOSE_CHAR: char = ')';
const STRING_BOUNDARY_CHAR: char = '"';
const SEMICOLON: char = ';';

lazy_static! {
    static ref SYMBOL_CHARS: HashSet<char> = vec![
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
        'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
        'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
        'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7',
        '8', '9', '-', '_', '$', '^', '&', '*', '!', '@', '%', '+',
        '?', '<', '>', '.', ':', '/', '|', '~', ',', '=',
    ].into_iter().collect();

    static ref TOKEN_BOUNDARY_CHARS: HashSet<char> = vec![
        EOL_CHAR,
        BACKSLASH_CHAR,
        PAREN_OPEN_CHAR,
        PAREN_CLOSE_CHAR,
        STRING_BOUNDARY_CHAR,
        SEMICOLON,
    ].into_iter().collect();
}

#[derive(Debug, Eq, PartialEq)]
pub enum LexerError {
    UnrecognizedCharacter(char),
    UnexpectedEndOfInput,
    InvalidNumericLiteral,
}

pub type TokenResult = result::Result<Token, LexerError>;

pub struct Tokenizer {
    scanner: Scanner,
    reached_eof: bool,
}

impl Tokenizer {
    pub fn new(scanner: Scanner) -> Tokenizer {
        Tokenizer {
            scanner: scanner,
            reached_eof: false,
        }
    }

    fn ignore_whitespace(&mut self) {
        loop {
            let c = self.scanner.get_char();
            if c.is_none() {
                break;
            }

            let c = c.unwrap();
            if !c.is_whitespace() || c == EOL_CHAR {
                self.scanner.unget_char();
                break;
            }
        }
    }

    fn match_advanced_token(&mut self, start: char) -> TokenResult {
        if self.must_match_numeric(start) {
            return self.match_numeric(start);
        }

        let mut symbol = String::new();

        let mut next_char = Some(start);
        while !next_char.is_none() {
            let c = next_char.unwrap();
            if SYMBOL_CHARS.contains(&c) {
                symbol.push(c)
            }
            else if self.is_token_boundary(&c) {
                self.scanner.unget_char();
                break;
            }
            else {
                return Err(LexerError::UnrecognizedCharacter(c));
            }

            next_char = self.scanner.get_char();
        }

        self.validate_symbol(symbol)
    }

    fn must_match_numeric(&mut self, start: char) -> bool {
        // Text must match a numeric literal if it could possibly
        // be interpreted as a number.
        // More formally, text must match a number if any of the
        // following conditions are met:
        // 1. start is a digit
        // 2. start is either '-' or '.' and the second character is a digit
        // 3. start is '-', the second character is '.' and the third character is a digit
        //TODO: There must be some way to optimize this significantly
        if start.is_digit(10) {
            return true;
        }

        let start_dash = start == '-';
        let start_dot = start == '.';

        let second = self.scanner.get_char();
        // Cannot undo this mutation right away just in case we need to get a third character after this

        let second_is_digit = second.map_or(false, |c| c.is_digit(10));
        
        if (start_dash || start_dot) && second_is_digit {
            // Undo any extra mutations that occurred
            if !second.is_none() { self.scanner.unget_char(); }
            return true;
        }

        let third = self.scanner.get_char();
        // Undo any extra mutations that occurred
        if !third.is_none() { self.scanner.unget_char(); }
        if !second.is_none() { self.scanner.unget_char(); }

        let second_is_dot = second.map_or(false, |c| c == '.');
        let third_is_digit = third.map_or(false, |c| c.is_digit(10));

        if start_dash && second_is_dot && third_is_digit {
            return true;
        }

        false
    }

    fn match_numeric(&mut self, start: char) -> TokenResult {
        let mut literal = String::new();
        literal.push(start);

        loop {
            let next_char = self.scanner.get_char();
            if next_char.is_none() {
                break;
            }

            let c = next_char.unwrap();
            // This is quite naive because parse() will do most of the work
            if c.is_digit(10) || c == '.' || c == 'e' || c == '-' {
                literal.push(c)
            }
            else if self.is_token_boundary(&c) {
                self.scanner.unget_char();
                break;
            }
            else {
                return Err(LexerError::InvalidNumericLiteral);
            }
        }

        literal.parse::<f64>()
            .map(|value| Token::Number(value))
            .map_err(|_| LexerError::InvalidNumericLiteral)
    }

    fn validate_symbol(&mut self, symbol: String) -> TokenResult {
        if symbol.is_empty() {
            Err(match self.scanner.get_char() {
                Some(c) => LexerError::UnrecognizedCharacter(c),
                None => LexerError::UnexpectedEndOfInput,
            })
        }
        else if symbol == "=" {
            Ok(Token::Equals)
        }
        else {
            Ok(Token::Symbol(symbol))
        }
    }

    fn is_token_boundary(&self, c: &char) -> bool {
        TOKEN_BOUNDARY_CHARS.contains(&c) || c.is_whitespace()
    }
}

impl Iterator for Tokenizer {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.reached_eof {
            return None;
        }

        self.ignore_whitespace();

        let c = self.scanner.get_char();
        if c.is_none() {
            self.reached_eof = true;
            return None;
        }

        let c = c.unwrap();
        Some(Ok(match c {
            BACKSLASH_CHAR => Token::Backslash,
            PAREN_OPEN_CHAR => Token::ParenOpen,
            PAREN_CLOSE_CHAR => Token::ParenClose,
            STRING_BOUNDARY_CHAR => Token::StringBoundary,
            SEMICOLON => Token::Semicolon,
            EOL_CHAR => Token::EOL,
            _ => return Some(self.match_advanced_token(c)),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use grammar::token::Token;
    use grammar::token::Token::*;
    use parser::scanner::Scanner;

    #[test]
    fn backslash() {
        test_single_token("\\", Backslash);
    }

    #[test]
    fn equals() {
        test_single_token("=", Equals);
    }

    #[test]
    fn paren_open() {
        test_single_token("(", ParenOpen);
    }

    #[test]
    fn paren_close() {
        test_single_token(")", ParenClose);
    }

    #[test]
    fn string_boundary() {
        test_single_token("\"", StringBoundary);
    }

    #[test]
    fn eol() {
        test_single_token("\n", EOL);
    }

    #[test]
    fn semicolon() {
        test_single_token(";", Semicolon);
    }

    #[test]
    fn number() {
        test_number("-5394", -5394f64);
        test_number("0.5", 0.5f64);
        test_number(".5", 0.5f64);
        test_number("-.5", -0.5f64);
        test_number("1233.14", 1233.14f64);
        test_number("3.14159e-3", 3.14159e-3f64);
    }

    #[test]
    fn groups_characters_into_symbols() {
        test_symbol("abqq$$^&/|!");
    }

    #[test]
    fn allows_symbols_starting_with_minus() {
        test_symbol("-abqq$$^&/|!");
    }

    #[test]
    fn groups_equals_into_symbols_when_there_is_no_whitespace() {
        test_symbol("=ifsaoi=hfsdMMDS,,,,~~~:=");
    }

    #[test]
    fn accepts_all_valid_symbol_characters_into_a_symbol() {
        test_symbol("abcdefghijklmnopqrstuvwxyzABCDEFGHIJ\
                    KLMNOPQRSTUVWXYZ0123456789-_$^&*!@%+?<>.:/|~,=");
    }

    #[test]
    fn recognizes_symbols_containing_numbers() {
        test_symbol("ms^-2");
    }

    #[test]
    fn separates_semicolon_from_the_rest() {
        test_expected_tokens(";- abc", &[
            Semicolon,
            Symbol("-".to_owned()),
            Symbol("abc".to_owned()),
        ]);
    }

    #[test]
    fn recognizes_multiple_tokens_separated_by_whitespace() {
        test_expected_tokens("\\ ) \n ( \\ = = )", &[
            Backslash,
            ParenClose,
            EOL,
            ParenOpen,
            Backslash,
            Equals,
            Equals,
            ParenClose
        ]);
    }

    #[test]
    fn allows_only_minus_in_symbol() {
        // This test is important because it may fail if numeric parsing changes
        test_expected_tokens("x - 1", &[
            Symbol("x".to_owned()),
            Symbol("-".to_owned()),
            Number(1f64),
        ]);
    }

    #[test]
    fn allows_only_minus_dot_in_symbol() {
        // This test is important because it may fail if numeric parsing changes since -.5 is a valid numeric literal
        test_expected_tokens("x -. 1", &[
            Symbol("x".to_owned()),
            Symbol("-.".to_owned()),
            Number(1f64),
        ]);
    }

    #[test]
    fn recognizes_parenthesis_regardless_of_whitespace() {
        test_expected_tokens("(x + y)())", &[
            ParenOpen,
            Symbol("x".to_owned()),
            Symbol("+".to_owned()),
            Symbol("y".to_owned()),
            ParenClose,
            ParenOpen,
            ParenClose,
            ParenClose
        ]);
    }

    #[test]
    fn recognizes_string_boundaries_regardless_of_whitespace() {
        test_expected_tokens("\"x + y\"\"\"\"", &[
            StringBoundary,
            Symbol("x".to_owned()),
            Symbol("+".to_owned()),
            Symbol("y".to_owned()),
            StringBoundary,
            StringBoundary,
            StringBoundary,
            StringBoundary
        ]);
    }

    #[test]
    fn disallows_symbols_starting_with_numbers() {
        test_symbol_error("123abc~", LexerError::InvalidNumericLiteral);
    }

    #[test]
    fn disallows_symbols_starting_with_numbers_and_minus() {
        test_symbol_error("-123abc~", LexerError::InvalidNumericLiteral);
    }

    #[test]
    fn disallows_invalid_numbers() {
        test_symbol_error("192.168.0.1", LexerError::InvalidNumericLiteral);
    }

    #[test]
    fn disallows_invalid_characters() {
        test_symbol_error("]", LexerError::UnrecognizedCharacter(']'));

        // Make sure invalid character is caught among valid characters too
        test_symbol_error("a[$abc~", LexerError::UnrecognizedCharacter('['));
    }

    #[test]
    fn ignores_leading_whitespace() {
        let symbol = "oh$*$ho".to_owned();
        let mut tokenizer = tokenizer_for(&format!("  \t\t  \t {}", symbol));
        assert_eq!(tokenizer.next().unwrap().unwrap(), Symbol(symbol.to_owned()));
        assert!(tokenizer.next().is_none());
    }

    // Test utility functions

    fn tokenizer_for(string: &str) -> Tokenizer {
        Tokenizer::new(Scanner::from_str(string))
    }

    fn test_single_token(input: &str, expected_token: Token) {
        let mut tokenizer = tokenizer_for(input);
        assert_eq!(tokenizer.next().unwrap().unwrap(), expected_token);
        assert!(tokenizer.next().is_none());
    }

    fn test_symbol(symbol: &str) {
        let mut tokenizer = tokenizer_for(symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap(), Symbol(symbol.to_owned()));
        // Ensure no extra tokens other than what is expected
        assert!(tokenizer.next().is_none());
    }

    fn test_symbol_error(symbol: &str, err: LexerError) {
        let mut tokenizer = tokenizer_for(symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap_err(), err);
        // Behaviour of tokens after an error are undefined
    }

    fn test_expected_tokens(input: &str, expected: &[Token]) {
        let mut tokenizer = tokenizer_for(input);

        for token in expected.into_iter() {
            assert_eq!(tokenizer.next().unwrap().unwrap(), *token);
        }

        assert!(tokenizer.next().is_none());
    }

    fn test_number(literal: &str, expected_value: f64) {
        const ERROR_MARGIN: f64 = 0.0;
        let mut tokenizer = tokenizer_for(literal);

        let matched_token = tokenizer.next().unwrap().unwrap();
        if let Number(value) = matched_token {
            assert!(value - expected_value <= ERROR_MARGIN,
                format!("The literal '{}' did not match the value {}, got {} instead", literal, expected_value, value));
        }
        else {
            panic!("Did not get Number when expecting {}, got {:?} instead", expected_value, matched_token);
        }

        assert!(tokenizer.next().is_none());
    }
}
