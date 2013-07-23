var assert = require('assert');

var anagrams = function(word) {
  if (word.length <= 1) {
    return [word];
  } else {
    var accum = [];

    var nextCases = drop1(word);

    for (var i=0;i<nextCases.length;++i) {
       var anagramsOfSubstrings = anagrams(nextCases[i]);

       for(var j=0;j<anagramsOfSubstrings.length;++j) {
         accum.push(word[i]+anagramsOfSubstrings[j]);
       }
    }

    return accum;
  }
};

var drop1 = function(word) {
  var withOneMissing = [];
  for (var i=0;i<word.length;++i) {
    withOneMissing.push(word.substr(0,i) + word.substr(i+1));
  }
  return withOneMissing;
};

var test = function() {

  // base case
  assert.deepEqual([''], anagrams(''));
  assert.deepEqual(['a'], anagrams('a'));

  var twoLetterAnagrams = anagrams('ab');
  assert.equal(2, twoLetterAnagrams.length);

  var dropLetter = drop1('abc');
  assert.deepEqual(['bc','ac','ab'], dropLetter);

  assert.deepEqual(['ab','ba'], twoLetterAnagrams);

  var threeLetterAnagrams = anagrams('abc');
  assert.deepEqual(['abc','acb','bac','bca', 'cab', 'cba'], threeLetterAnagrams);
}

test();