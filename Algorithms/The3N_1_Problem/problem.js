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

var cycleLength = function(n) {
  return basic(n).length;
};


var memo = function(f) {

  var table = {};
  return function(n) {
    if (!table[n]) {
      table[n] = f(n);
    }

    return table[n];
  };
};


describe('3n + 1', function() {
  describe('basics', function() {
    it('returns all values', function() {
      assert.deepEqual(
        [22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1],
        basic(22)
      );
    });

    it('calculates the cycle length', function() {
      assert.equal(16, cycleLength(22));
    });
  });

  describe('memoizer', function() {
    it('memoizes', function() {
      var callCount = 0;

      var memoed = memo(function(n) { callCount++; return n; });

      memoed(8);
      assert.equal(1, callCount);

      memoed(8);
      assert.equal(1, callCount);
    });
  });
});
