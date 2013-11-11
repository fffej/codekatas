"use strict";

var assert = require('assert');

var add = function(x,y) { return x + y; };

var Game = function() {

    var pins = [];

    this.roll = function(n) {	
	pins.push(n);
    };

    this._scoreSpare = function(pins, i) {
	return pins[i] + pins[i+1] + pins[i+2];
    };

    this.score = function() {

	var sum = 0;

	var pinCount = 0;
	for (var i=0;i<10;++i) {	    

	    if (pins[pinCount] + pins[pinCount+1] === 10) {
		sum += this._scoreSpare(pins,pinCount);
	    }
	    else {
		sum += pins[pinCount] + pins[pinCount+1];
	    }

	    pinCount += 2;
	}

	return sum;
    };

    return this;
};

describe('bowling game', function() {

    var rollMany = function(game, roll, n) {
	for (var i=0;i<n;++i) {
	    game.roll(roll);
	}
    };

    it('rolling 20 gutter balls is a gutter game', function() {
	var game = new Game();
	rollMany(game, 0, 20);

	assert.equal(0, game.score());
    });

    it('hitting 20 single pins scores 20', function() {
	var game = new Game();
	rollMany(game, 1, 20);

	assert.equal(20, game.score());
    });

    it('handles spares', function() {
	var game = new Game();
	game.roll(5);
	game.roll(5); // spare
	game.roll(3);

	// Rest is a gutter game
	rollMany(game, 0,17);

	assert.equal(16, game.score());
    });
});
