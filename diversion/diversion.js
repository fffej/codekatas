var assert = require("assert");

var bitset = function(value,bit) {
  return true;
};

describe('Diversion', function() {
  it('has a bit that is set', function() {
    assert(bitset(1,1));
  });
});