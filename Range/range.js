"use strict";

var assert = require('assert');

var ClosedInterval = function(from,to) {
  return {
    minimum: function() { return from; }
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
    it('is created', function() {
      assert(createClosedInterval(1,4));
    });

    it('minimum', function() {
      var range = createClosedInterval(1,4);
      assert.equal(1, range.minimum());
    });
  });
});
