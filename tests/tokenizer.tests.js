const {expect} = require('chai');

const Tokenizer = require('../src/tokenizer'); 

describe('Tokenizer', () => {
  it('ignores whitespace at the beginning of an expression', () => {
  });

  it('uses exclusively whitespace to group tokens', () => {
    // x^2 != x ^ 2
  });

  it('accepts all characters valid for symbols', () => {
  });

  it('rejects all characters invalid for symbols', () => {
  });

  it('it accepts all basic operators as valid symbols', () => {
    // +, -, *, /, ^, ++, --, ==, >=, <=
  });
});
