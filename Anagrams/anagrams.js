"use strict";

var assert = require('assert');

var anagrams = function(s) {
    var n = s.length;

    if (n === 0) return [];

    if (n === 1) return [s];


    var r = [];

    for (var i=0;i<n;++i) {
	var rest = s.substr(0,i) + s.substr(i+1);
	r.push(s[i] + rest);
    }
    
    return r;
};

describe('anagrams', function() {

    it('anagrams of an empty string are empty', function() {
	assert.deepEqual([], anagrams(''));
    });

    it('anagrams of a single character', function() {
	assert.deepEqual(['a'], anagrams('a'));
	assert.deepEqual(['z'], anagrams('z'));
    });

    it('anagrams of multiple characters', function() {
	assert.deepEqual(['aa','aa'], anagrams('aa'));
	assert.deepEqual(['bb','bb'], anagrams('bb'));
    });

    it('anagrams of different characters', function() {
	assert.deepEqual(['ab','ba'], anagrams('ab'));
    });

    it('anagrams have expected count', function() {
	assert.equal(5*4*3*2*1, anagrams('abcde').length);
    });
});
