use std::iter::Iterator;
use std::result;
use std::collections::HashSet;

use token::Token;
use scanner::Scanner;

const EOL_CHAR: char = '\n';
const BACKSLASH_CHAR: char = '\\';
const PAREN_OPEN_CHAR: char = '(';
const PAREN_CLOSE_CHAR: char = ')';
const STRING_BOUNDARY_CHAR: char = '"';

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
    ].into_iter().collect();
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenError {
    UnrecognizedCharacter(char),
    UnexpectedEndOfInput,
    InvalidNumericLiteral,
}

pub type TokenResult = result::Result<Token, TokenError>;

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
                return Err(TokenError::UnrecognizedCharacter(c));
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
                return Err(TokenError::InvalidNumericLiteral);
            }
        }

        literal.parse::<f64>()
            .map(|value| Token::Number(value))
            .map_err(|_| TokenError::InvalidNumericLiteral)
    }

    fn validate_symbol(&mut self, symbol: String) -> TokenResult {
        if symbol.len() == 0 {
            Err(match self.scanner.get_char() {
                Some(c) => TokenError::UnrecognizedCharacter(c),
                None => TokenError::UnexpectedEndOfInput,
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
            EOL_CHAR => Token::EOL,
            _ => return Some(self.match_advanced_token(c)),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use token::Token::*;
    use scanner::Scanner;

    #[test]
    fn backslash() {
        let mut tokenizer = tokenizer_for("\\");
        assert_eq!(tokenizer.next().unwrap().unwrap(), Backslash);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn equals() {
        let mut tokenizer = tokenizer_for("=");
        assert_eq!(tokenizer.next().unwrap().unwrap(), Equals);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn paren_open() {
        let mut tokenizer = tokenizer_for("(");
        assert_eq!(tokenizer.next().unwrap().unwrap(), ParenOpen);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn paren_close() {
        let mut tokenizer = tokenizer_for(")");
        assert_eq!(tokenizer.next().unwrap().unwrap(), ParenClose);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn string_boundary() {
        let mut tokenizer = tokenizer_for("\"");
        assert_eq!(tokenizer.next().unwrap().unwrap(), StringBoundary);
        assert!(tokenizer.next().is_none());
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
    fn eol() {
        let mut tokenizer = tokenizer_for("\n");
        assert_eq!(tokenizer.next().unwrap().unwrap(), EOL);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn recognizes_multiple_tokens_separated_by_whitespace() {
        let mut tokenizer = tokenizer_for("\\ ) \n ( \\ = = )");
        let expected = [Backslash, ParenClose, EOL, ParenOpen, Backslash, Equals, Equals, ParenClose];

        for token in expected.into_iter() {
            assert_eq!(tokenizer.next().unwrap().unwrap(), *token);
        }

        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn groups_characters_into_symbols() {
        let symbol = "abqq$$^&/|!";
        let mut tokenizer = tokenizer_for(symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap(), Symbol(symbol.to_owned()));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn allows_symbols_starting_with_minus() {
        let symbol = "-abqq$$^&/|!";
        let mut tokenizer = tokenizer_for(symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap(), Symbol(symbol.to_owned()));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn allows_only_minus_in_symbol() {
        // This test is important because it may fail if numeric parsing changes
        let mut tokenizer = tokenizer_for("x - 1");
        let expected = [
            Symbol("x".to_owned()),
            Symbol("-".to_owned()),
            Number(1f64),
        ];

        for token in expected.into_iter() {
            assert_eq!(tokenizer.next().unwrap().unwrap(), *token);
        }
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn allows_only_minus_dot_in_symbol() {
        // This test is important because it may fail if numeric parsing changes since -.5 is a valid numeric literal
        let mut tokenizer = tokenizer_for("x -. 1");
        let expected = [
            Symbol("x".to_owned()),
            Symbol("-.".to_owned()),
            Number(1f64),
        ];

        for token in expected.into_iter() {
            assert_eq!(tokenizer.next().unwrap().unwrap(), *token);
        }
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn groups_equals_into_symbols_when_there_is_no_whitespace() {
        let symbol = "=ifsaoi=hfsdMMDS,,,,~~~:=".to_owned();

        // Make sure leading whitespace does not effect the output
        let mut tokenizer = tokenizer_for(&format!("  {}", symbol));

        assert_eq!(tokenizer.next().unwrap().unwrap(), Symbol(symbol));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn disallows_symbols_starting_with_numbers() {
        let symbol = "123abc~".to_owned();
        let mut tokenizer = tokenizer_for(&symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap_err(), TokenError::InvalidNumericLiteral);
    }

    #[test]
    fn disallows_symbols_starting_with_numbers_and_minus() {
        let symbol = "-123abc~".to_owned();
        let mut tokenizer = tokenizer_for(&symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap_err(), TokenError::InvalidNumericLiteral);
    }

    #[test]
    fn disallows_invalid_numbers() {
        let symbol = "192.168.0.1".to_owned();
        let mut tokenizer = tokenizer_for(&symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap_err(), TokenError::InvalidNumericLiteral);
    }

    #[test]
    fn disallows_invalid_characters() {
        let symbol = "]";
        let mut tokenizer = tokenizer_for(&symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap_err(), TokenError::UnrecognizedCharacter(']'));

        // Make sure invalid character is caught among valid characters too
        let symbol = "a[$abc~".to_owned();
        let mut tokenizer = tokenizer_for(&symbol);

        assert_eq!(tokenizer.next().unwrap().unwrap_err(), TokenError::UnrecognizedCharacter('['));
    }

    #[test]
    fn accepts_all_valid_symbol_characters_into_a_symbol() {
        let symbol = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_$^&*!@%+?<>.:/|~,=".to_owned();
        let mut tokenizer = tokenizer_for(&symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap(), Symbol(symbol));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn recognizes_symbols_containing_numbers() {
        let symbol = "ms^-2";
        let mut tokenizer = tokenizer_for(symbol);
        assert_eq!(tokenizer.next().unwrap().unwrap(), Symbol(symbol.to_owned()));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn recognizes_parenthesis_regardless_of_whitespace() {
        let mut tokenizer = tokenizer_for("(x + y)())");
        let expected = [ParenOpen, Symbol("x".to_owned()), Symbol("+".to_owned()), Symbol("y".to_owned()), ParenClose, ParenOpen, ParenClose, ParenClose];

        for token in expected.into_iter() {
            assert_eq!(tokenizer.next().unwrap().unwrap(), *token);
        }

        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn recognizes_string_boundaries_regardless_of_whitespace() {
        let mut tokenizer = tokenizer_for("\"x + y\"\"\"\"");
        let expected = [StringBoundary, Symbol("x".to_owned()), Symbol("+".to_owned()), Symbol("y".to_owned()), StringBoundary, StringBoundary, StringBoundary, StringBoundary];

        for token in expected.into_iter() {
            assert_eq!(tokenizer.next().unwrap().unwrap(), *token);
        }

        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn ignores_leading_whitespace() {
        let symbol = "oh$*$ho".to_owned();
        let mut tokenizer = tokenizer_for(&format!("  \t\t  \t {}", symbol));
        assert_eq!(tokenizer.next().unwrap().unwrap(), Symbol(symbol.to_owned()));
        assert!(tokenizer.next().is_none());
    }

    fn tokenizer_for(string: &str) -> Tokenizer {
        Tokenizer::new(Scanner::from_str(string))
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
