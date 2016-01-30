const r_whitespace = /[ \t\r\f\v]/;

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

  ignoreWhitespace() {
    while (r_whitespace.test(this.getChar())) {}
    this.ungetChar();
  }
}

module.exports = Scanner;
