"use strict";

var assert = require('assert');

var deadCell = function() {
    return {};
};

var liveCell = function() {
    return { 
	next: function(n) { 
	    return deadCell();
	}
    };
};

describe('game of life', function() {
    describe('rules', function() {
	it('under population', function() {
	    assert.deepEqual(deadCell(), liveCell().next(1));
	});

	it('persistence', function() {
	    assert.deepEqual(liveCell(), liveCell().next(2));
	});
    });
});
