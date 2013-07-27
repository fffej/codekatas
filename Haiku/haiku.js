"use strict";
var assert = require('assert');

var syllableCount = function(word) {
  var syllables = 0;

  var inSyllableRun = false;

  for (var i=0;i<word.length;++i) {
    if (isVowel(word[i]) && !inSyllableRun) {
      syllables++;
      inSyllableRun = true;
    } else {
      inSyllableRun = false;
    }
  }

  return syllables;
};

var isVowel = function(c) {
  return "aeiouy".indexOf(c) !== -1;
};

describe('haiku', function() {
  describe('syllables', function() {
    it('contiguous sequence of vowels is one syllable', function() {
      assert.equal(1, syllableCount('o'));
      assert.equal(1, syllableCount('oa'));
      assert.equal(1, syllableCount('at'));
      assert.equal(1, syllableCount('bab'));
    });

    it('broken by consonants is multiple syllables', function() {
      assert.equal(2, syllableCount('aba'));
      assert.equal(3, syllableCount('banana'));
      assert.equal(2, syllableCount('code'));
    });

    it('should split sentences into words', function() {
      assert.deepEqual(['the','quick','brown','fox'],
                       splitWords('the quick brown fox'));
    });

    it('should have an understanding of vowels', function() {
      var vowels = "aeiouy";
      for (var i=0;i<vowels.length;++i) {
        assert(isVowel(vowels[i]));
      }

      var consonants = "bcdfghjklmnpqrstvwxz";
      for (var i=0;i<consonants.length;++i) {
        assert(!isVowel(consonants[i]));
      }
    });
  });
});
