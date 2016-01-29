const {expect} = require('chai');

const Scanner = require('../src/scanner.js'); 

describe('Scanner', () => {
    const SMALL_TEXT = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut convallis turpis velit,\nid ullamcorper nulla\ntristique vel. Morbi vel massa in lacus euismod facilisis.\nEtiam quis est velit. Suspendisse\nmolestie ultricies lectus, in vehicula neque\nfaucibus eget. Phasellus\nvenenatis tincidunt massa. Mauris\nlaoreet luctus\nfinibus. Donec sit amet quam\nquis."

  it('works with no text', () => {
    const scanner = new Scanner();

    const position = scanner.position;
    expect(scanner.getChar()).to.equal(Scanner.EOF);
    expect(scanner.position).to.equal(position);

    scanner.ungetChar();
    expect(scanner.position).to.equal(position);
  });

  it('works with an empty string', () => {
    const scanner = new Scanner('');

    const position = scanner.position;
    expect(scanner.getChar()).to.equal(Scanner.EOF);
    expect(scanner.position).to.equal(position);

    scanner.ungetChar();
    expect(scanner.position).to.equal(position);
  });

  it('should get characters accurately', () => {
    const scanner = new Scanner(SMALL_TEXT);

    for (let i = 0; i < SMALL_TEXT.length; i++) {
      const c = scanner.getChar();
      expect(c).to.equal(SMALL_TEXT[i]);
      expect(scanner.position).to.equal(i);
    }

    expect(scanner.getChar()).to.equal(Scanner.EOF);
  });

  it('should unget characters correctly', () => {
    const scanner = new Scanner(SMALL_TEXT);

    for (let i = 0; i < SMALL_TEXT.length; i++) {
      const c = scanner.getChar();
    }

    expect(scanner.getChar()).to.equal(Scanner.EOF);
    expect(scanner.position).to.equal(SMALL_TEXT.length-1);
    for (let i = SMALL_TEXT.length-1; i >= 0; i--) {
      expect(scanner.position).to.equal(i);

      scanner.ungetChar();
      expect(scanner.getChar()).to.equal(SMALL_TEXT[i]);

      scanner.ungetChar();
    }
  });
});
