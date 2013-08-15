"use strict";
var assert = require('assert');

var basic = function(n) {
  var values = [];
  while (n !== 1) {
    values.push(n);
    if (n % 2 === 0) {
      n /= 2;
    } else {
      n = n * 3 + 1;
    }
  }
  values.push(1);
  return values;
};

describe('3n + 1', function() {
  describe('basics', function() {
    it('returns all values', function() {
      assert.deepEqual(
        [22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1],
        basic(22)
      );
    });
  });
});
