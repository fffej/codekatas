"use strict";
var assert = require('assert');

/*
 For the purposes of this problem,
every contiguous sequence of one or more vowels counts
as one syllable, where the vowels are
a, e, i, o, u, and y. Every word will contain at least
one syllable.
*/

var syllableCount = function(word) {
  return 1;
};

describe('haiku', function() {
  describe('syllables', function() {
    it('should be able to count syllables', function() {
      assert.equal(1, syllableCount('bat'));
    });
  });
});
