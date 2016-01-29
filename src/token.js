class Token {
  static SYMBOL = "symbol";
  static EQUALS = "equals";
  static ARROW = "arrow";
  static PARENOPEN = "paren-open";
  static PARENCLOSE = "paren-close";

  constructor(type, value=null) {
    this.type = type;
    this.value = value;
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
}

module.exports = Token;
