"use strict";

var assert = require('assert');


var largestString = function(words) {

};

describe('words smushing', function() {
  describe('largest string', function() {
    it('is not defined for 0 length arrays', function() {
      assert(!largestString([]));
    });

    it('returns the length of the longest string', function() {
      assert.equal(5, largestString['a','b','bc','defgh']);
    });
  });
});
