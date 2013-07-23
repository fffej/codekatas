var assert = require('assert');

var anagrams = function(word) {
  if (word.length <= 1) {
    return [word];
  } else {

  }
};

var test = function() {

  // base case
  assert.deepEqual([''], anagrams(''));
  assert.deepEqual(['a'], anagrams('a'));

  var twoLetterAnagrams = anagrams('ab');
  assert.equal(1, twoLetterAnagrams.length);
}

test();