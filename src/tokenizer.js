const Scanner = require('./scanner');
const Token = require('./token');

const {UnexpectedTokenError} = require('./errors');

// represents a valid character for use in the token SYMBOL
const rSymbol = /[a-z0-9\-_$^&*!@%+?<>.:/|~,=]/i;

class Tokenizer {
  constructor(text='') {
    this.scanner = new Scanner(text);
  }

  /**
   * Searches for and returns the next token
   */
  next() {
    this.scanner.ignoreWhitespace();

    let text = "";
    let c;
    while (rSymbol.test(c = this.scanner.getChar())) {
      text += c;
    }
    this.scanner.ungetChar();

    if (!text.length) {
      return Token.eof();
    }

    switch (text) {
      case "=":
        return Token.equals();
      case "=>":
        return Token.arrow();
      default:
        return Token.symbol(text);
    }
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
