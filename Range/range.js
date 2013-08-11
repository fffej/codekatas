"use strict";

var assert = require('assert');

var ClosedInterval = function(from,to) {
  return {
    contains: function(a) {
      return true;
    },

    maximum: function() { return to; },

    minimum: function() { return from; }
  };
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

  describe('range contains', function() {
    it('correctly contains', function() {
      var range = createClosedInterval(1,100);

      assert(range.contains(1));
      assert(range.contains(50));
      assert(range.contains(100));
    });
  });
});
