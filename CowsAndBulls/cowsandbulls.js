"use strict";

var assert = require('assert');

var Game = function(goal) {

    var scoreBulls = function(guess, goal) {
	var bulls = 0;
	for (var i=0;i<guess.length;++i) {
	    if (guess[i] === goal[i])
		bulls++;
	}
	return bulls;
    };

    var scoreCows = function(guess, goal) {
	var cows = 0;
	for (var i=0;i<goal.length;++i) {
	    var desiredChar = goal[i];
	    for (var j=0;j<goal.length;++j) {
		if (j === i) continue;

		if (guess[j] === desiredChar) {
		    cows++;
		}
	    }
	}
	return cows;
    };

    this.score = function(guess) {
	assert(goal.length === guess.length, 'Lengths must be equivalent');

        return {
	    cows: scoreCows(guess,goal),
	    bulls: scoreBulls(guess,goal)
	};
    };

    var finishingScore = function(score) {
	return score.bulls === 4;
    };

    this.play = function(strategy) {
	var turns = 0;

	do {
	    turns++;
	    var guess = strategy.guess();
	    var score = this.score(guess);
	    strategy.update(score);
	} while (!finishingScore(score));

	return turns;
    };

    return this;
};

var asString = function(num) {
    var s = '0000' + num;
    return s.substr(s.length - 4);
};

var fromTo = function(start,end) {
    var arr = [];
    for (var i=start;i<=end;++i) {
	arr.push(i);
    }
    return arr;
};

var Player = function() {

    this.possibilities = fromTo(0,9999);

    this.guess = function() {
	return asString(1234);
    };

    this.update = function(guess,score) {
	if (score.cows === 0) {
	    // None of the numbers are correct
	    for (var i=0;i<guess.length;++i) {
		var digit = guess[i];
		for (var k=this.possibilities.length-1;k>=0;--k) {
		    var possibility = this.possibilities[k];
		    if (asString(possibility).indexOf(digit) !== -1) {
			this.possibilities.splice(k,1);
		    }
		}
	    }
	}
    };

    return this;
};

var Naive = function() {

    this.currentGuess = 0;


    this.guess = function() {
	return asString(this.currentGuess);
    };

    this.update = function(guess, score) {
	this.currentGuess++;
    };

    return this;
};

var createGame = function(goal) {
    return new Game(goal);
};

var containsDuplicates = function(str) {
    for (var i=0;i<str.length;++i) {
	var found = false;
	for (var j=0;j<str.length;++j) {
	    if (i === j) continue;
	    
	    if (str[i] === str[j])
		return true;
	}
    }
    return false;
};

describe("cows and bulls", function() {

    describe('playing naive', function() {
	it('should solve it within 9999 turns', function() {
	    var game = createGame('5555');
	    var turns = game.play(new Naive());

	    assert.equal(5556, turns);
	});
    });

    describe('intelligently playing', function() {


	it('should have four unique numbers in initial guess', function() {
	    var player = new Player();

	    var initialGuess = player.guess();
	    assert(!containsDuplicates(initialGuess), 'no duplicates');
	});

	it('initial pool of answers is 10000', function() {
	    var player = new Player();

	    assert.equal(10000, player.possibilities.length);
	});

	it('guesses eliminate possibilities', function() {
	    var player = new Player();

	    player.update('1111', { cows: 0, bulls: 0 });

	    assert.equal(6561, player.possibilities.length);
	});
    });

    describe('scoring', function() {
	it('should match the example', function() {
	    var game = createGame('8045');
	    var result = game.score('0865');

	    assert.equal(2, result.cows);
	    assert.equal(1, result.bulls);
	});

	it('no matches scores no cows or bulls', function() {
            var game = createGame('1234');

            var r = game.score('5678');
            assert.equal(0,r.cows);
	    assert.equal(0,r.bulls);
	});

	it('all correct is all bulls', function() {
	    var game = createGame('1234');

	    var r = game.score('1234');
	    assert.equal(4, r.bulls);
	    assert.equal(0, r.cows);
	});

	it('all in wrong order is all cows', function() {
	    var game = createGame('1234');

	    var r = game.score('4321');
	    assert.equal(4, r.cows);
	    assert.equal(0, r.bulls);
	});
    });
});
