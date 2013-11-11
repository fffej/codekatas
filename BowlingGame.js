"use strict";

var assert = require('assert');

var add = function(x,y) { return x + y; };

var Game = function() {

    var pins = [];

    this.roll = function(n) {	
	pins.push(n);
    };

    this.score = function() {
	return pins.reduce(add,0);
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
});
