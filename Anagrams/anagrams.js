var assert = require('assert');

var anagrams = function(word) {
  return [''];
};

var test = function() {

  assert.deepEqual([''], anagrams(''));

  assert.deepEqual(['a'], anagrams('a'));
}

test();