"use strict";

var assert = require('assert');

var deadCell = function() {

    var states = [
	deadCell,
	deadCell,
	deadCell,
	liveCell,
	deadCell,
	deadCell,
	deadCell,
	deadCell,
	deadCell
    ];

    return {
	dead: true,
	next: function(n) {
	    return states[n]();
	}
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
	    for (var i=0;i<2;++i) 
		assert(liveCell().next(i).dead);
	});

	it('persistence', function() {
	    assert(liveCell().next(2).live);
	    assert(liveCell().next(3).live);
	});

	it('over population', function() {
	    for (var i=4;i<9;++i)
		assert(liveCell().next(i).dead);
	});

	it('produce', function() {
	    assert(deadCell().next(3).live);
	});

	it('dead stays dead', function() {
	    for (var i=0;i<3;++i)
		assert(deadCell().next(i).dead);

	    for (var i=4;i<9;++i)
		assert(deadCell().next(i).dead);
	});
    });
});
