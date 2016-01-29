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
}

module.exports = Scanner;
