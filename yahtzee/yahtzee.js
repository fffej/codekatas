"use strict";

var assert = require('assert');

var Categories = {
    Chance: 0,
    Ones:   1
};

var score = function(category, a,b,c,d,e) {
    return a + b + c + d + e;
};

describe('yahtzee', function() {
    describe('scoring', function() {
	it('should score chance as the sum of the dice', function() {
	    assert.equal(1+2+3+4+5, score(Categories.Chance, 1,2,3,4,5));
	});

	it('should score \'ones\' as the sum of those matching ones', function() {
	    assert.equal(3, score(Categories.Ones, 1,1,1,2,3));
	});
    });
});
