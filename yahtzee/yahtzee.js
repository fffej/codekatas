"use strict";

var assert = require('assert');

var score = function(a,b,c,d,e) {

};

describe('yahtzee', function() {
    describe('scoring', function() {
	it('should score change as the sum of the dice', function() {
	    assert.equal(1+2+3+4+5, score(1,2,3,4,5));
	});
    });
});
