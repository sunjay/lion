const {expect} = require('chai');

const Tokenizer = require('../src/tokenizer'); 

describe('Tokenizer', () => {
  it('allows you to iterate over the tokens in a string', () => {
    const tokens = new Tokenizer('a b c');
    expect(() => Array.from(tokens)).to.not.throw(Error);
  });

  it('ignores whitespace at the beginning of an expression', () => {
    //const tokens = new Tokenizer('      \t \t    a');
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

  it('accepts all basic operators as valid symbols', () => {
    // +, -, *, /, ^, ++, --, ==, >=, <=
    expect(1).to.equal(2);
  });

  it('allows newlines to be inserted appropriately in expressions', () => {
    expect(1).to.equal(2);
  });

  it('allows you to get each token one at a time correctly', () => {
    expect(1).to.equal(2);
  });

  it('allows you to search for a given token and get everything inbetween', () => {
    expect(1).to.equal(2);
  });

  it('allows you to assert that the next token is what you want when you are getting it', () => {
    expect(1).to.equal(2);
  });
});
