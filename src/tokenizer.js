const Scanner = require('./scanner');
const Token = require('./token');

class Tokenizer {
  constructor(text='') {
    this.scanner = new Scanner(text);
  }
}

module.exports = Tokenizer;
