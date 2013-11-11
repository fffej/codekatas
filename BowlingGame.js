"use strict";

var assert = require('assert');

var Game = function() {
    this.roll = function(n) {	
    };

    this.score = function() {
	return 0;
    };

    return this;
};

describe('bowling game', function() {
    it('rolling 20 gutter balls is a gutter game', function() {
	var game = new Game();
	for (var i=0;i<20;++i) {
	    game.roll(0);
	}

	assert.equal(0, game.score());
    });
});
