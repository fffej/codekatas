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

  this.max = function(x,y) {
    var longest = 0;
    for (var i=x;i<=y;++i) {
      longest = Math.max(longest, this.cycleLength(i));
    }
    return longest;
  };

  this.cycleLength = function(n) {
    var c = 1;
    var org = n;

    while (n !== 1) {
      if (this.table[n]) {
        return (c-1) + this.table[n];
      }      
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
      var originalCallCount = callCount;

      assert.equal(15, chain.cycleLength(11));
      assert.equal(originalCallCount, callCount);
    });

    it('can calculate the max in range', function() {
      var chain = new Chain();
      assert.equal(20, chain.max(1,10));
      assert.equal(125, chain.max(100,200));
      assert.equal(89, chain.max(201,210));
      assert.equal(174, chain.max(900,1000));
    });

    it('handles big numbers', function() {
      var chain = new Chain();
      assert.equal(525, chain.max(1,1000000));
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
