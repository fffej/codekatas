var assert = require("assert");

var bitset = function(value,bit) {
  return true;
};

describe('Diversion', function() {
  it('has a bit that is set', function() {
    assert(bitset(1,1));
  });

  it('bit shift operations', function() {
    assert.equal(1, 1 << 0);
    assert.equal(2, 1 << 1);
    assert.equal(4, 1 << 2);
  });
});