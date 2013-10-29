"use strict";

var assert = require('assert');

var calculateScore = function(guess, secret) {
    return {
	cows: 0,
	bulls: 0
    };
};

describe('cows and bulls', function() {
    describe('scoring', function() {
	it('scores nothing when no matches', function() {
	    var score = calculateScore('1234', '5678');

	    assert.equal(0, score.cows);
	    assert.equal(0, score.bulls);
	});

	it('scores bulls when all matches', function() {
	    var score = calculateScore('1234', '1234');
	    
	    assert.equal(0, score.cows);
	    assert.equal(4, score.bulls);
	});
    });
});
