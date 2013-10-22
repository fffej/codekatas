"use strict";

var assert = require('assert');

var calculateScore = function(guess, secret) {
    var cows = 0;
    var bulls = 0;

    for (var i=0;i<secret.length;++i) {
	var found = guess.indexOf(secret[i]);
	if (found === i) 
	    bulls++
	else if (found !== -1)
	    cows++;
    }

    return { cows: cows, bulls: bulls };
};

var Player = function() {

    this.guesses = ['5678'];

    this.guess = function() {
	return this.guesses.pop();
    };
    
    return this;
};

describe('cows and bulls', function() {
    describe('scoring', function() {
	var assertCowsAndBulls = function(expected, actual) {
	    assert.equal(actual.cows, expected.cows);
	    assert.equal(actual.bulls, expected.bulls);
	};

	it('no matches scores zero cows and bulls', function() {
	    var score = calculateScore('1234','5678');
	    assertCowsAndBulls({cows: 0, bulls: 0}, score);
	});
	
	it('single match in wrong place scores one cows', function() {
	    var score = calculateScore('1234', '5671');
	    assertCowsAndBulls({cows: 1, bulls: 0}, score);
	});

	it('single match in right place scores one bull', function() {
	    var score = calculateScore('1234', '1567');
	    assertCowsAndBulls({cows: 0, bulls: 1}, score);
	});
    });

    describe('strategy', function() {
	it('guesses have four characters', function() {
	    var player = new Player();
	    assert.equal(4, player.guess().length);
	});
    });
});
