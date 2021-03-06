"use strict";

var assert = require('assert');

var add = function(x,y) { return x + y; };

var Game = function() {

    this._pins = [];

    this.roll = function(n) {	
	this._pins.push(n);
    };

    this._scoreSpare = function(i) {
	return {
	    score: this._scoreFrame(i).score + this._pins[i+2],
	    pinsConsumed: 2
	};
    };

    this._scoreFrame = function(i) {
	return {
	    score: this._pins[i] + this._pins[i+1],
	    pinsConsumed: 2
	};
    };

    this._isSpare = function(i) {
	return this._pins[i] + this._pins[i+1] === 10;
    };

    this._isStrike = function(i) {
	return this._pins[i] === 10;
    };

    this._scoreStrike = function(i) {
	return {
	    score: 10 + this._pins[i+1] + this._pins[i+2],
	    pinsConsumed: 1
	};
    };

    this.score = function() {

	var sum = 0;
	var pinCount = 0;

	for (var i=0;i<10;++i) {
	    var result;
	    if (this._isStrike(i)) {
		result = this._scoreStrike(pinCount);
	    }
	    else if (this._isSpare(pinCount)) {
		result = this._scoreSpare(pinCount);
	    }
	    else {
		result = this._scoreFrame(pinCount);
	    }
	    
	    sum += result.score;
	    pinCount+= result.pinsConsumed;
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

    var rollSpare = function(game) {
	game.roll(5);
	game.roll(5);
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
	rollSpare(game);
	game.roll(3);

	// Rest is a gutter game
	rollMany(game, 0,17);

	assert.equal(16, game.score());
    });

    it('handles strikes', function() {
	var game = new Game();
	game.roll(10);
	game.roll(3);
	game.roll(3);

	rollMany(game, 0, 16);

	assert.equal(10 + 6 + 6, game.score());
    });

    it('handles a perfect game', function() {
	var game = new Game();
	rollMany(game, 10, 12);

	assert.equal(300, game.score());
    });
});
