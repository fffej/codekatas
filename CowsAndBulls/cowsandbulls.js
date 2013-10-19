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

    this.score = function(guess) {
	assert(goal.length === guess.length, 'Lengths must be equivalent');

	var cows = 0;
        var bulls = scoreBulls(guess,goal);

        return {
	    cows: cows,
	    bulls: bulls
	};
    };
    return this;
};

var createGame = function(goal) {
    return new Game(goal);
};

describe("cows and bulls", function() {
    describe('scoring', function() {
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
	});
    });
});
