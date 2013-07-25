var assert = require("assert");

var bitset = function(value,bit) {
  return !!(value & (1 << bit));
};

var bitsSet = function(value) {
  var bits = numberOfBits(value);
  var ret = [];
  for (var i=0;i<bits;++i) {
    ret.push(bitset(value,i) ? 1 : 0);
  }
  return ret;
};

var numberOfBits = function(value) {
  if (value === 0) {
    return value;
  }

  var log2 = Math.log(value) / Math.LN2;
  return (Math.pow(2,log2) >= value) ? log2 : log2 + 1;
};

describe('Diversion', function() {
  it('has a bit that is set', function() {
    assert(bitset(1,0));
    assert(bitset(256,8));
  });

  it('number of bits within a value', function() {
    assert.equal(8, numberOfBits(256));
    assert.equal(0, numberOfBits(0));
  });

  it('has bits that are not set', function() {
    assert(!bitset(257,7));
  });

  it('returns the binary representation', function() {
    assert.deepEqual([], bitsSet(0));
    assert.deepEqual([0,1], bitsSet(2));
  });

  it('bit shift operations', function() {
    assert.equal(1, 1 << 0);
    assert.equal(2, 1 << 1);
    assert.equal(4, 1 << 2);
  });
});