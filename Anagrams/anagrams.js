"use strict";

var assert = require('assert');

var anagrams = function() {
    return ['a'];
};

describe('anagrams', function() {
    it('anagrams of a single character', function() {
	var anagramsOfA = anagrams('a');

	assert.deepEqual(['a'], anagramsOfA);
    });
});
