"use strict";

var assert = require('assert');

var ClosedInterval = function(from,to) {

  this.contains = function(a) {
      if (a instanceof ClosedInterval) {
        var min = a.minimum();
        var max = a.maximum();

        return this.contains(min) && this.contains(max);
      } else {
        return a >= from && a <= to;
      }
  };

  this.maximum = function() {
    return to;
  };

  this.minimum = function() {
    return from;
  };

  return this;
};

var createClosedInterval = function(from,to) {
  return new ClosedInterval(from, to);
};

var createOpenInterval = function(from,to) {
  return new ClosedInterval(from+1, to-1);
};

describe('range', function() {
  describe('creation', function() {
    it('exists', function() {
      assert(new ClosedInterval);
    });
  });

  describe('closed intervals', function() {
 
    var defaultRangeForTests = createClosedInterval(1,4);

    it('is created', function() {
      assert(defaultRangeForTests);
    });

    it('minimum', function() {
      assert.equal(1, defaultRangeForTests.minimum());
    });

    it('maximum', function() {
      assert.equal(4, defaultRangeForTests.maximum());
    });
  });

  describe('open intervals', function() {
    var defaultRangeForTests = createOpenInterval(1,4);

    it('is created', function() {
      assert(defaultRangeForTests);
    });

    it('minimum', function() {
      assert.equal(2, defaultRangeForTests.minimum());
    });

    it('maximum', function() {
      assert.equal(3, defaultRangeForTests.maximum());
    });
  });

  describe('closed range contains', function() {

    var range = createClosedInterval(1,100);

    it('correctly contains', function() {
      assert(range.contains(1));
      assert(range.contains(50));
      assert(range.contains(100));
    });

    it('correctly doesn\'t contain', function() {
      assert.equal(false, range.contains(0));
      assert.equal(false, range.contains(101));
    });

    it('can contain smaller ranges', function() {
      assert(range.contains(createClosedInterval(2,99)));
    });
  });

  describe('open range contains', function() {
    
    var range = createOpenInterval(1,100);

    it('correctly contains', function() {
      assert(range.contains(2));
      assert(range.contains(50));
      assert(range.contains(99));
    });

    it('correctly doesn\'t contain', function() {
      assert.equal(false, range.contains(1));
      assert.equal(false, range.contains(100));
    });
  });
});
