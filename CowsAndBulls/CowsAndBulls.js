"use strict";

var assert = require('assert');

var calculateCows = function(guess, secret) {
    var n = guess.length;
    var cows = 0;

    for (var i=0;i<n;++i) {
	var idx = secret.indexOf(guess[i]);
	if (idx !== -1 && idx !== i) 
	    cows++;
    }

    return cows;
};

var calculateBulls = function(guess, secret) {
    var n = guess.length;
    var bulls = 0;

    for (var i=0;i<n;++i)
	if (guess[i] === secret[i])
	    bulls++;

    return bulls;
};

var calculateScore = function(guess, secret) {
    return {
	cows: calculateCows(guess,secret),
	bulls: calculateBulls(guess,secret)
    };
};

var calculateInitialGuesses = function() {
    var digits = ['0','1','2','3','4','5','6','7','8','9'];

    var permutations = function(n) {
	if (n === 1) return digits;

	var rest = permutations(n - 1);
	var options = [];
	for (var i=0;i<rest.length;++i) {
	    for (var j=0;j<digits.length;++j) {
		if (rest[i].indexOf(digits[j]) === -1) {
		    options.push(digits[j] + rest[i]);
		}
	    }
	}

	return options;
    };

    return permutations(4);
};

var scoreEquals = function(a,b) {
    return a.cows === b.cows && a.bulls === b.bulls;
};

var eliminateGuesses = function(guesses, guess, score) {
    var rest = [];
    var n = guesses.length;

    for (var i=0;i<n;++i) {
	var sc = calculateScore(guesses[i],guess);
	if (scoreEquals(sc,score))
	    rest.push(guesses[i]);
    }
    

    return rest;
};

describe('cows and bulls', function() {

    describe('eliminations', function() {
	it('should eliminate all not matching', function() {
	    var guesses = calculateInitialGuesses();

	    var rest = eliminateGuesses(guesses, '1234', {bulls: 0, cows: 0});

	    assert.equal(6*5*4*3, rest.length);
	});
    });

    describe('permutations', function() {
	it('should have 10*9*8*7 initial guesses', function() {
	    var setOfGuesses = calculateInitialGuesses();

	    assert.equal(10*9*8*7, setOfGuesses.length);
	});
    });

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

	it('scores cows when permutation', function() {
	    var score = calculateScore('1234', '4321');
	    
	    assert.equal(4, score.cows);
	    assert.equal(0, score.bulls);
	});

	it('scores partially', function() {
	    var score = calculateScore('1234', '1243');

	    assert.equal(2, score.cows);
	    assert.equal(2, score.bulls);
	});
    });
});
