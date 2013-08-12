"use strict";

var assert = require('assert');


var largestString = function(words) {
  if (words && words.length > 1) {
    return words.reduce(function(x,y) {
      return Math.max(x,y.length); 
    }, words[0].length);
  }
};

describe('words smushing', function() {
  describe('largest string', function() {
    it('is not defined for 0 length arrays', function() {
      assert(!largestString([]));
    });

    it('returns the length of the longest string', function() {
      assert.equal(5, largestString(['a','b','bc','defgh']));
    });
  });

  describe('eliminate wholly contains substrings', function() {
    it('duplicates are eliminated', function() {
      assert.equal(1, eliminateSubstrings(['test','testing']).length);
    });
  });
});
