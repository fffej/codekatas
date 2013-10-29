"use strict";

var assert = require('assert');

describe('cows and bulls', function() {
    describe('scoring', function() {
	it('scores nothing when no matches', function() {
	    var score = calculateScore('1234', '5678');

	    assert.equal(0, score.cows);
	    assert.equal(0, score.bulls);
	});
    });
});
