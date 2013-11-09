"use strict";

var assert = require('assert');

describe('game of life', function() {
    describe('rules', function() {
	it('under population', function() {
	    var cell = liveCell();
	    assert.equal(deadCell(), liveCell().next(1));
	});
    });
});
