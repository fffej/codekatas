"use strict";

var assert = require('assert');

var ClosedInterval = function(from,to) {
  return {
    minimum: function() { return from; },
    maximum: function() { return to; }
  };
};

var createClosedInterval = function(from,to) {
  return new ClosedInterval(from, to);
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
});
