const {expect} = require('chai');

const Tokenizer = require('../src/tokenizer'); 

describe('Tokenizer', () => {
  it('ignores whitespace at the beginning of an expression', () => {
    expect(1).to.equal(2);
  });

  it('uses exclusively whitespace to group tokens', () => {
    // x^2 != x ^ 2
    expect(1).to.equal(2);
  });

  it('allows the use of parenthesis with or without whitespace', () => {
    expect(1).to.equal(2);
  });

  it('allows the use of backslash with or without whitespace', () => {
    expect(1).to.equal(2);
  });

  it('distinguishes between equals surrounded by symbol characters and equals surrounded by whitespace', () => {
    // ` = 2` is not ` =2`
    expect(1).to.equal(2);
  });

  it('accepts all characters valid for symbols', () => {
    expect(1).to.equal(2);
  });

  it('rejects all characters invalid for symbols', () => {
    expect(1).to.equal(2);
  });

  it('it accepts all basic operators as valid symbols', () => {
    // +, -, *, /, ^, ++, --, ==, >=, <=
    expect(1).to.equal(2);
  });
});
