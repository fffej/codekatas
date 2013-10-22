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

var generateGuesses = function(n) {
    var digits = ['0','1','2','3','4','5','6','7','8','9'];
    if (n === 1) return digits;

    var rest = generateGuesses(n-1);
    var guesses = [];
    for (var i=0;i<rest.length;++i)
	for (var d=0;d<digits.length;++d) 
	    if (rest[i].indexOf(digits[d]) === -1)
		guesses.push(digits[d] + rest[i]);

    return guesses;
};

var Player = function() {

    this.guesses = generateGuesses(4);

    this.guess = function() {
	return this.guesses.pop();
    };
    
    return this;
};

var Game = function(secret) {
    
    this.play = function(player) {
	var turns = 0;

	do {
	    var guess = player.guess();
	    var score = calculateScore(guess, secret);
	    turns++;
	} while (score.bulls !== 4);

	return turns;
    };
    return this;
};

describe('cows and bulls', function() {

    var fact = function(n) {
	if (n === 1) return 1;
	return n * fact(n-1);
    };

    var possibilities = fact(10) / fact(10-4);

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

	it('initially contains all possibilities', function() {
	    var player = new Player();
	    assert.equal(possibilities, player.guesses.length);
	});
    });

    describe('playing', function() {
	it('always wins in the end', function() {
	    var game = new Game('5678');
	    var turnsTaken = game.play(new Player());

	    assert(turnsTaken < possibilities);
	});
    });
});
