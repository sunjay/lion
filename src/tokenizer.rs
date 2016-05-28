use std::iter::Iterator;
use std::collections::HashSet;

use token::Token;
use scanner::Scanner;

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
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use token::Token;
    use token::Token::*;
    use scanner::Scanner;

    #[test]
    fn backslash() {
        let mut tokenizer = tokenizer_for("\\");
        assert!(tokenizer.next().unwrap() == Backslash);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn equals() {
        let mut tokenizer = tokenizer_for("=");
        assert!(tokenizer.next().unwrap() == Equals);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn parenOpen() {
        let mut tokenizer = tokenizer_for("(");
        assert!(tokenizer.next().unwrap() == ParenOpen);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn parenClose() {
        let mut tokenizer = tokenizer_for(")");
        assert!(tokenizer.next().unwrap() == ParenClose);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn eol() {
        let mut tokenizer = tokenizer_for("\n");
        assert!(tokenizer.next().unwrap() == EOL);
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn recognizes_multiple_tokens_separated_by_whitespace() {
        let mut tokenizer = tokenizer_for("\\ ) \n ( \\ = = )");
        let expected = [Backslash, ParenClose, EOL, ParenOpen, Backslash, Equals, Equals, ParenClose];

        for token in expected.into_iter() {
            assert!(tokenizer.next().unwrap() == *token);
        }

        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn groups_characters_into_symbols() {
        let symbol = "abqq$$1111^&/|!".to_string();
        let mut tokenizer = tokenizer_for(symbol);
        assert!(tokenizer.next().unwrap() == Symbol(symbol));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn groups_equals_into_symbols_when_there_is_no_whitespace() {
        let symbol = "=ifsaoi=hfsdMMDS,,,,~~~:=";

        // Make sure leading whitespace does not effect the output
        let tokens = tokenizer_for("  " + symbol);

        assert!(tokenizer.next().unwrap() == Symbol(symbol));
        assert!(tokenizer.next().is_none());
    }

    #[test]
    fn disallows_symbols_starting_with_numbers() {
    }

    #[test]
    fn accepts_all_valid_symbol_characters_into_a_symbol() {
    }

    #[test]
    fn recognizes_parenthesis_regardless_of_whitespace() {
    }

    #[test]
    fn ignores_leading_whitespace() {
    }

    fn tokenizer_for(string: &str) -> Tokenizer {
        Tokenizer::new(Scanner::from_str(string))
    }
}


