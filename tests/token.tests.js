const {expect} = require('chai');

const Token = require('../src/token'); 

describe('Token', () => {
  // These are mostly just intended as sanity checks since this is
  // such a simple class
  describe('is created correctly for', () => {
    it('symbol', () => {
      testToken('symbol', Token.SYMBOL);
      testToken('symbol', Token.SYMBOL, 'test symbol text');
    });

    it('equals', () => {
      testToken('equals', Token.EQUALS);
    });

    it('backslash', () => {
      testToken('backslash', Token.BACKSLASH);
    });

    it('parenopen', () => {
      testToken('parenopen', Token.PARENOPEN);
    });

    it('parenclose', () => {
      testToken('parenclose', Token.PARENCLOSE);
    });

    it('eof', () => {
      testToken('eof', Token.EOF);
    });

    function testToken(methodName, type, value=null) {
      const token = Token[methodName](value);
      expect(token.type).to.equal(type);
      expect(token.value).to.equal(value);
    }
  });
});
