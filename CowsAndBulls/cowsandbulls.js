"use strict";

var assert = require('assert');

var calculateScore = function(guess, secret) {
    var cows = 0;
    var bulls = 0;

    for (var i=0;i<secret.length;++i) {
	var secretChar = secret[i];
	if (guess.indexOf(secretChar) !== -1) 
	    cows++;
    }

    return { cows: cows, bulls: bulls };
};

describe('cows and bulls', function() {
    describe('scoring', function() {
	it('no matches scores zero cows and bulls', function() {
	    var score = calculateScore('1234','5678');
	    
	    assert.equal(score.cows, 0);
	    assert.equal(score.bulls, 0);
	});
	
	it('single match in wrong place scores one cows', function() {
	    var score = calculateScore('1234', '5671');
	    
	    assert.equal(score.cows, 1);
	    assert.equal(score.bulls, 0);
	});
    });
});
