"use strict";

var assert = require('assert');

var anagrams = function(s) {
    return [s];
};

describe('anagrams', function() {

    it('anagrams of a single character', function() {
	assert.deepEqual(['a'], anagrams('a'));
	assert.deepEqual(['z'], anagrams('z'));
    });
});
