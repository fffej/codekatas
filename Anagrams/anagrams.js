"use strict";

var assert = require('assert');

var anagrams = function(s) {
    var n = s.length;

    if (n === 1) {
	return [s];
    }
    
    
};

describe('anagrams', function() {

    it.only('anagrams of a single character', function() {
	assert.deepEqual(['a'], anagrams('a'));
	assert.deepEqual(['z'], anagrams('z'));
    });

    it('anagrams of multiple characters', function() {
	assert.deepEqual(['aa','aa'], anagrams('aa'));
    });
});
