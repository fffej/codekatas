"use strict";

var assert = require('assert');

var deadCell = function() {
    return {
	dead: true
    };
};



var liveCell = function() {

    var states = [
	deadCell,
	deadCell,
	liveCell,
	liveCell,
	deadCell,
	deadCell,
	deadCell,
	deadCell,
	deadCell
    ];

    return { 
	live: true,
	next: function(n) { 
	    return states[n]();
	}
    };
};

describe('game of life', function() {
    describe('rules', function() {
	it('under population', function() {
	    assert(liveCell().next(1).dead);
	});

	it('persistence', function() {
	    assert(liveCell().next(2).live);
	});

	it('over population', function() {
	    assert(liveCell().next(4).dead);
	});
    });
});
