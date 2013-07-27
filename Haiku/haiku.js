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

var splitWords = function(words) {
  return words.split(' ');
};

var splitPhrase = function(phrase) {
  return phrase.split('/');
};

var countSyllables = function(words) {
  var s = splitWords(words);
  var cs = s.map(syllableCount);

  return cs.reduce(function(x,y) { return x + y } );
};

var syllables = function(phrase) {
  var lines = splitPhrase(phrase);
  return lines.map(countSyllables);
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

    it('should count syllables in a phrase', function() {
      assert.equal(4, countSyllables('the quick brown fox'));
    });

    it('should split on / for phrases', function() {
      assert.deepEqual(['one two three', 'four five', 'six seven'],
                       splitPhrase('one two three/four five/six seven'));
    
    });

    it('should get syllable counts for a phrase', function() {
      var example1 = 'happy purple frog/eating bugs in the marshes/get indigestion';
      
       assert.deepEqual([5,7,5], syllables(example1));
    });

    it('should know the syllable counts for a haiku', function() {
      assert(isHaikuCount([5,7,5]));
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
