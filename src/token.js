class Token {
  static SYMBOL = "symbol";
  static EQUALS = "equals";
  static ARROW = "arrow";
  static PARENOPEN = "paren-open";
  static PARENCLOSE = "paren-close";
  static EOF = "eof";

  constructor(type, value=null) {
    this.type = type;
    this.value = value;
  }

  /**
   * Returns true if the given token is of the given type
   */
  static is(token, type) {
    return token.type === type;
  }

  /**
   * Returns true if the given token represents the end of the file
   */
  static isEOF(token) {
    return this.is(token, this.EOF);
  }

  static symbol(value) {
    return new Token(Token.SYMBOL, value);
  }

  static equals() {
    return new Token(Token.EQUALS);
  }

  static arrow() {
    return new Token(Token.ARROW);
  }

  static parenopen() {
    return new Token(Token.PARENOPEN);
  }

  static parenclose() {
    return new Token(Token.PARENCLOSE);
  }

  static eof() {
    return new Token(Token.EOF);
  }
}

module.exports = Token;
