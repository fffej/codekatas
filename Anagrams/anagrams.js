"use strict";

var assert = require('assert');

var anagrams = function() {
    return ['a'];
};

describe('anagrams', function() {

    it('anagrams of a single character', function() {
	assert.deepEqual(['a'], anagrams('a'));
	assert.deepEqual(['z'], anagrams('z'));
    });
});
