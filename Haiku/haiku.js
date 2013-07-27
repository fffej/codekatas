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

var isVowel = function(c) {
  return "aeiouy".indexOf(c) !== -1;
};

describe('haiku', function() {
  describe('syllables', function() {
    it('contiguous sequence of vowels is one syllable', function() {
      assert.equal(1, syllableCount('oeee'));
    });

    it('should have an understanding of vowels', function() {
      var vowels = "aeiouy";
      for (var i=0;i<vowels.length;++i) {
        assert(isVowel(vowels[i]));
      }
    });
  });
});
