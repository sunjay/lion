const rWhitespace = /[ \t\r\f\v]/;

class Scanner {
  static EOF = "EOF";

  constructor(text='') {
    this.text = text;
    this.position = -1;
  }

  getChar() {
    if (this.position >= this.text.length-1) {
      return Scanner.EOF;
    }
    this.position++;
    return this.text[this.position];
  }

  ungetChar() {
    if (this.position < 0) {
      return;
    }
    this.position--;
  }

  /**
   * Ignores all whitespace other than newlines
   */
  ignoreWhitespace() {
    while (Scanner.isWhitespace(this.getChar())) {}
    this.ungetChar();
  }

  static isWhitespace(c) {
    return rWhitespace.test(c);
  }
}

module.exports = Scanner;
