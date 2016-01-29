export class UnexpectedTokenError extends Error {
  constructor(expected, actual) {
    super(`Expected a '${expected}' but saw a '${actual}' instead.`);
  }
}
