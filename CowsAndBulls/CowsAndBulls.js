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

var Game = function(secret) {
    this.play = function(player) {
	var turns = 0;
	do {
	    var guess = player.guess();
	    var score = calculateScore(secret,guess);
	    player.update(guess,score);
	    turns++;
	} while (score.bulls !== 4)

	return turns;
    };
    return this;
};

var ComputerPlayer = function() {
    this.guesses = calculateInitialGuesses();

    this.guess = function() {
	return this.guesses.pop();
    };

    this.update = function(guess,score) {
	this.guesses = eliminateGuesses(this.guesses,guess,score);
    };

    return this;
};

describe('cows and bulls', function() {

    describe('game', function() {
	it('should complete within 10 turns', function() {
	    var game = new Game('5432');
	    
	    var turnsTaken = game.play(new ComputerPlayer());
	    
	    assert(turnsTaken < 10);
	});
    });

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

	var assertScoreEquals = function(score,expected) {
	    assert(scoreEquals(score,expected));
	};

	it('scores nothing when no matches', function() {
	    var score = calculateScore('1234', '5678');

	    assertScoreEquals({cows: 0, bulls: 0}, score);
	});

	it('scores bulls when all matches', function() {
	    var score = calculateScore('1234', '1234');
	    
	    assertScoreEquals({cows: 0, bulls: 4}, score);
	});

	it('scores cows when permutation', function() {
	    var score = calculateScore('1234', '4321');
	    
	    assertScoreEquals({cows: 4, bulls: 0}, score);
	});

	it('scores partially', function() {
	    var score = calculateScore('1234', '1243');

	    assertScoreEquals({cows: 2, bulls: 2}, score);
	});
    });
});
