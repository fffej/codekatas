"use strict";

var assert = require('assert');

var Range = function() {
  
};

var createCloseRange = function(from,to) {
  return new Range();
};

describe('range', function() {
  describe('creation', function() {
    it('exists', function() {
      assert(new Range);
    });
  });

  describe('closed intervals', function() {
    it('is created', function() {
      assert(createCloseRange(1,4));
    });

    it('minimum', function() {
      var range = createCloseRange(1,4);
      assert.equal(2, range.minimum());
    });
  });
});
