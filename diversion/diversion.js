var assert = require("assert");

var bitset = function(value,bit) {
  return !!(value & (1 << bit));
};

var bitsSet = function(value) {
  return [0];
};

describe('Diversion', function() {
  it('has a bit that is set', function() {
    assert(bitset(1,0));
    assert(bitset(256,8));
  });

  it('has bits that are not set', function() {
    assert(!bitset(257,7));
  });

  it('returns the binary representation', function() {
    assert.deepEqual([0], bitsSet(0));
    assert.deepEqual([0,1], bitsSet(2));
  });

  it('bit shift operations', function() {
    assert.equal(1, 1 << 0);
    assert.equal(2, 1 << 1);
    assert.equal(4, 1 << 2);
  });
});