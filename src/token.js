class Token {
  static SYMBOL = "symbol";
  static EQUALS = "equals";
  static BACKSLASH = "backslash";
  static PARENOPEN = "paren-open";
  static PARENCLOSE = "paren-close";
  static EOF = "eof";

  constructor(type, value=null) {
    this.type = type;
    this.value = value;
  }

  toString() {
    let str = `${this.type.toUpperCase()}`;
    if (this.value && !(this.value instanceof Symbol)) {
      str += `('${this.value}')`;
    }
    return str;
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

  static backslash() {
    return new Token(Token.BACKSLASH);
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
