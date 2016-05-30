use std::iter::Iterator;
use std::result;
use std::collections::HashSet;

use token::Token;
use scanner::Scanner;

const EOL_CHAR: char = '\n';

lazy_static! {
    static ref SYMBOL_CHARS: HashSet<char> = vec![
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
        'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
        'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
        'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7',
        '8', '9', '-', '_', '$', '^', '&', '*', '!', '@', '%', '+',
        '?', '<', '>', '.', ':', '/', '|', '~', ',', '='
    ].into_iter().collect();
}

#[derive(Debug, Eq, PartialEq)]
pub enum TokenError {
    UnrecognizedCharacter(char),
    InvalidSymbol,
    ScannerError(String),
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

    fn ignore_whitespace(&mut self) -> result::Result<(), &str> {
        loop {
            let char = self.scanner.get_char();
            if char.is_none() {
                break;
            }

            let char = char.unwrap();
            if !char.is_whitespace() || char == EOL_CHAR {
                try!(self.scanner.unget_char());
                break;
            }
        }

        Ok(())
    }
}

impl Iterator for Tokenizer {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.reached_eof {
            return None;
        }

        if let Err(message) = self.ignore_whitespace() {
            return Some(Err(TokenError::ScannerError(message.to_owned())));
        }

        None
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
        assert!(tokenizer.next().unwrap().unwrap() == Backslash);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn equals() {
        let mut tokenizer = tokenizer_for("=");
        assert!(tokenizer.next().unwrap().unwrap() == Equals);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn paren_open() {
        let mut tokenizer = tokenizer_for("(");
        assert!(tokenizer.next().unwrap().unwrap() == ParenOpen);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn paren_close() {
        let mut tokenizer = tokenizer_for(")");
        assert!(tokenizer.next().unwrap().unwrap() == ParenClose);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn paren_close() {
        let mut tokenizer = tokenizer_for("\"");
        assert!(tokenizer.next().unwrap().unwrap() == StringBoundary);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn eol() {
        let mut tokenizer = tokenizer_for("\n");
        assert!(tokenizer.next().unwrap().unwrap() == EOL);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn recognizes_multiple_tokens_separated_by_whitespace() {
        let mut tokenizer = tokenizer_for("\\ ) \n ( \\ = = )");
        let expected = [Backslash, ParenClose, EOL, ParenOpen, Backslash, Equals, Equals, ParenClose];

        for token in expected.into_iter() {
            assert!(tokenizer.next().unwrap().unwrap() == *token);
        }

        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn groups_characters_into_symbols() {
        let symbol = "abqq$$1111^&/|!";
        let mut tokenizer = tokenizer_for(symbol);
        assert!(tokenizer.next().unwrap().unwrap() == Symbol(symbol.to_owned()));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn groups_equals_into_symbols_when_there_is_no_whitespace() {
        let symbol = "=ifsaoi=hfsdMMDS,,,,~~~:=".to_owned();

        // Make sure leading whitespace does not effect the output
        let mut tokenizer = tokenizer_for(&format!("  {}", symbol));

        assert!(tokenizer.next().unwrap().unwrap() == Symbol(symbol));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn disallows_symbols_starting_with_numbers() {
        let symbol = "123abc~".to_owned();
        let mut tokenizer = tokenizer_for(&symbol);
        assert!(tokenizer.next().unwrap().unwrap_err() == TokenError::InvalidSymbol);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn disallows_invalid_characters() {
        let symbol = "]";
        let mut tokenizer = tokenizer_for(&symbol);
        assert!(tokenizer.next().unwrap().unwrap_err() == TokenError::UnrecognizedCharacter(']'));
        assert!(tokenizer.next().is_none());

        // Make sure invalid character is caught among valid characters too
        let symbol = "12[3abc~".to_owned();
        let mut tokenizer = tokenizer_for(&symbol);
        assert!(tokenizer.next().unwrap().unwrap_err() == TokenError::UnrecognizedCharacter('['));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn accepts_all_valid_symbol_characters_into_a_symbol() {
        let symbol = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_$^&*!@%+?<>.:/|~,=".to_owned();
        let mut tokenizer = tokenizer_for(&symbol);
        assert!(tokenizer.next().unwrap().unwrap() == Symbol(symbol));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn recognizes_parenthesis_regardless_of_whitespace() {
        let mut tokenizer = tokenizer_for("(x + y)())");
        let expected = [ParenOpen, Symbol("x".to_owned()), Symbol("+".to_owned()), Symbol("y".to_owned()), ParenClose, ParenOpen, ParenClose, ParenClose];

        for token in expected.into_iter() {
            assert!(tokenizer.next().unwrap().unwrap() == *token);
        }

        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn recognizes_string_boundaries_regardless_of_whitespace() {
        let mut tokenizer = tokenizer_for("\"x + y\"\"\"\"");
        let expected = [StringBoundary, Symbol("x".to_owned()), Symbol("+".to_owned()), Symbol("y".to_owned()), StringBoundary, StringBoundary, StringBoundary, StringBoundary];

        for token in expected.into_iter() {
            assert!(tokenizer.next().unwrap().unwrap() == *token);
        }

        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn ignores_leading_whitespace() {
        let symbol = "oh#*$ho".to_owned();
        let mut tokenizer = tokenizer_for(&format!("  \t\t  \t {}", symbol));
        assert!(tokenizer.next().unwrap().unwrap() == Symbol(symbol.to_owned()));
        assert!(tokenizer.next().is_none());
    }

    fn tokenizer_for(string: &str) -> Tokenizer {
        Tokenizer::new(Scanner::from_str(string))
    }
}


