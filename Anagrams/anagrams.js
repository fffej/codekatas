var assert = require('assert');

var anagrams = function(word) {
  if (word.length <= 1) {
    return [word];
  } else {
    var accum = [];
    for(var i=0;i<word.length;++i) {
      accum.push(anagrams(word[i]));
    }

    return accum;
  }
};

var test = function() {

  // base case
  assert.deepEqual([''], anagrams(''));
  assert.deepEqual(['a'], anagrams('a'));

  var twoLetterAnagrams = anagrams('ab');
  assert.equal(2, twoLetterAnagrams.length);

  var dropLetter = drop1('abc');
  assert.deepEqual(['bc','ac','ab'], dropLetter);
}

test();