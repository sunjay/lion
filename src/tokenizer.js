const Scanner = require('./scanner');
const Token = require('./token');

const {InvalidCharacterError, UnexpectedTokenError} = require('./errors');

// represents a valid character for use in the token SYMBOL
const rSymbol = /[a-z0-9-_$^&*!@%+?<>.:/|~,=]/i;

class Tokenizer {
  constructor(text='') {
    this.scanner = new Scanner(text);
  }

  /**
   * Searches for and returns the next token
   */
  next() {
    this.scanner.ignoreWhitespace();

    const text = this.scanner.getChar();
    switch (text) {
      case "=":
          const nextChar = this.scanner.getChar();
          this.scanner.ungetChar();
          if (Scanner.isWhitespace(nextChar)) {
            return Token.equals();
          }
          // fallthrough and continue checking
      case "\\":
        return Token.backslash();
      case "(":
        return Token.parenopen();
      case ")":
        return Token.parenclose();
      case Scanner.EOF:
        return Token.eof();
    }

    this.scanner.ungetChar();
    return this._symbol();
  }

  _symbol() {
    let text = "";
    while (true) {
      const c = this.scanner.getChar();
      if (!rSymbol.test(c)) {
        this.scanner.ungetChar();
        break;
      }

      text += c;
    }

    if (!text.length) {
      throw new InvalidCharacterError("Invalid character");
    }

    return Token.symbol(text);
  }

  /**
   * Searches for a specific token type and returns all tokens
   * up to and including that one or throws an error if
   * the given token was not actually found
   */
  search(tokenType) {
    const found = [];
    for (let token of this) {
      found.push(token);

      if (Token.is(token, tokenType)) {
        return found;
      }

      if (Token.isEOF(token)) {
        throw new UnexpectedTokenError(tokenType, token.type);
      }
    }
  }

  /**
   * Searches for and returns the next token, throwing
   * an error if the token's type does not match tokenType
   */
  match(tokenType) {
    const token = this.next();
    if (!Token.is(token, tokenType)) {
      throw new UnexpectedTokenError(tokenType, token.type);
    }
    return token;
  }

  /**
   * Iterates over every token from the current position
   */
  *[Symbol.iterator]() {
    while (true) {
      const token = this.next();
      yield token;

      if (Token.isEOF(token)) {
        break;
      }
    }
  }
}

module.exports = Tokenizer;
