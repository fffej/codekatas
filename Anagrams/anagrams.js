"use strict";

var assert = require('assert');

var anagrams = function(s) {
    var n = s.length;

    if (n === 0) return [];

    if (n === 1) return [s];


    var r = [];

    for (var i=0;i<n;++i) {
	var remaining = s.substr(0,i) + s.substr(i+1);
	assert (remaining.length + 1 === s.length);
	var rest = anagrams(remaining);
	var jn = rest.length;
	for (var j=0;j<jn;++j) {
	    r.push(s[i] + rest[j]);
	}
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
