"use strict";

var assert = require('assert');

describe("cows and bulls", function() {
    describe('scoring', function() {
	it('no matches scores no cows or bulls', function() {
            var game = createGame('1234');

            var r = game.score('5678');
            assert.equal(0,r.cows);
	    assert.equal(0,r.bulls);
	});
    });
});
