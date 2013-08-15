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

var memo = function(f) {

  var table = {};
  return function(n) {
    if (!table[n]) {
      table[n] = f(n);
    }

    return table[n];
  };
};

var Chain = function() {

  this.table = {};

  this.next = function(n) {
    return (n % 2 === 0) ? n/2 : (n*3+1);
  };

  this.cycleLength = function(n) {
    var c = 1;
    var org = n;

    while (n !== 1) {
      c++;
      n = this.next(n);
    }

    this.table[org] = c;

    var n = org;
    while (n !== 1) {
      c--;
      n = this.next(n);
      if (!this.table[n]) {
        this.table[n] = c;
      } else {
        break;
      }
    }

    return this.table[org];    
  };

  return this;
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

  describe('table stores all values', function() {
    it('keeps score', function() {
      var chain = new Chain();
      assert.equal(16, chain.cycleLength(22));
    });

    it('stores intermediate values', function() {
      var chain = new Chain();
      chain.cycleLength(22);

      assert.equal(16, chain.table[22], '22');
      assert.equal(15, chain.table[11], '11');
    });

    it('does not recalculate values', function() {
      var chain = new Chain();
      var oldNext = chain.next;
      var callCount = 0;
      chain.next = function(n) {
        callCount++;
        return oldNext(n);
      }

      assert.equal(16, chain.cycleLength(22));
      assert(16, callCount);
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
