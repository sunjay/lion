use std::vec::Vec;

pub struct Scanner {
    position: isize,
    buffer: Vec<char>,
}

impl Scanner {
    pub fn new() -> Scanner {
        Scanner {
            position: -1,
            buffer: Vec::new(),
        }
    }

    pub fn from_str(string: &str) -> Scanner {
        let mut scanner = Scanner::new();
        scanner.push_str(string);
        scanner
    }

    pub fn len(&self) -> isize {
        self.buffer.len() as isize
    }

    pub fn push_str(&mut self, string: &str) {
        self.buffer.extend(string.chars());
    }

    pub fn get_char(&mut self) -> Option<char> {
        if self.position < self.len() - 1 {
            self.position += 1;

            if self.position < self.len() {
                let c = self.buffer[self.position as usize];
                return Some(c)
            }
        }

        None
    }

    pub fn unget_char(&mut self) {
        if self.position >= 0 {
            self.position -= 1;
        }
        else {
            panic!("No character to unget");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() {
        let scanner = Scanner::new();
        assert_eq!(scanner.len(), 0);
    }

    #[test]
    fn can_add_text() {
        let mut scanner = Scanner::new();
        let text = "This is some text";

        scanner.push_str(text);
        scanner.push_str(text);

        assert_eq!(scanner.len(), text.len() as isize * 2);
    }

    #[test]
    fn can_get_characters() {
        // A string with uppercase and lowercase letters where all are unique
        let text = "Total";
        let mut scanner = Scanner::from_str(text);
        
        for c in text.chars() {
            if let Some(next) = scanner.get_char() {
                assert!(next == c, format!("Got character `{}` instead of character `{}`", next, c));
            }
            else {
                panic!("Could not get expected number of characters. Stopped when expecting `{}`", c);
            }
        }
    }

    #[test]
    fn cannot_get_beyond_length() {
        let mut scanner = Scanner::new();
        assert!(scanner.get_char().is_none());

        scanner = Scanner::from_str("T");
        scanner.get_char();
        assert!(scanner.get_char().is_none());
        // try again just in case
        assert!(scanner.get_char().is_none());
    }

    #[test]
    fn can_unget_characters() {
        let mut scanner = Scanner::from_str("abc");
        // 1. Get up to the expected character
        // position -> 0
        scanner.get_char();
        // position -> 1
        let expected = scanner.get_char();

        // 2. Unget back to before any character
        // position -> 0
        scanner.unget_char();
        // position -> -1
        scanner.unget_char();

        // 3. Get up to the end
        // position -> 0
        scanner.get_char();
        // position -> 1
        scanner.get_char();
        // position -> 2
        scanner.get_char();

        // 4. Unget back to the expected character
        // position -> 1
        scanner.unget_char();
        // position -> 0
        scanner.unget_char();
        // position -> 1
        let actual = scanner.get_char();

        assert_eq!(expected, actual);
    }

    #[test]
    #[should_panic(expected = "unget")]
    fn cannot_unget_before_start() {
        let mut scanner = Scanner::from_str("T");
        // panic
        scanner.unget_char();
    }

    #[test]
    #[should_panic(expected = "unget")]
    fn cannot_unget_too_far() {
        let mut scanner = Scanner::from_str("T");

        scanner.get_char();
        scanner.unget_char();

        // panic
        scanner.unget_char();
    }
}
