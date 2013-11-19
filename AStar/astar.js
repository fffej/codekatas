"use strict";

var assert = require('assert');

var cost = function(c) {
    if (c === '^') return 3;

    if (c === '*') return 2;

    return 1;
};

var p = function(x,y) {
    return {
	x: x,
	y: y
    };
};

var manhattanDistance = function(p1,p2) {
    return Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y);
};

var Map = function(rows) {
    this.rows = rows;

    var findIt = function(rows, it) {
	for (var y=0;y<rows.length;++y) 
	    for (var x=0;x<rows[y].length;++x)
		if (rows[y][x] === it) 
		    return p(x,y);
    };

    this.goal = function() { 
	return findIt(this.rows, 'X');
    };

    this.start = function() { 
	return findIt(this.rows, '@');
    };

    this.moves = function(p) {
	var choices = [];

	return choices;
    };

    return this;
};

var load = function(s) {
    return new Map(s.split('\n'));
};

describe('a* search algorithm', function() {

    describe('map', function() {

	var defaultMap = '@.\n.X';

	it('finds goal', function() {
	    var map = load(defaultMap);
	    var p = map.goal();

	    assert.equal(1, p.x);
	    assert.equal(1, p.y);
	});

	it('finds start', function() {
	    var map = load(defaultMap);
	    var p = map.start();

	    assert.equal(0, p.x);
	    assert.equal(0, p.y);
	});

	it.skip('finds choices', function() {
	    var map = load(defaultMap);
	    
	    var choices = map.moves(p);

	    assert.equal(2, choices.length);
	});
    });

    describe('costs', function() {
	it('flatlands cost 1', function() {
	    assert.equal(1, cost('.'));
	    assert.equal(1, cost('@'));
	    assert.equal(1, cost('X'));
	});

	it('forest costs 2', function() {
	    assert.equal(2, cost('*'));
	});

	it('mountain costs 3', function() {
	    assert.equal(3, cost('^'));
	});

	it('walkable', function() {
	    assert(walkable('.'));
	    assert(walkable('*'));
	    assert(walkable('^'));
	    assert(walkable('@'));
	    assert(walkable('X'));
	});
    });

    describe('Manhattan distance', function() {
	it('works in a straight line', function() {
	    assert.equal(5, manhattanDistance(p(0,0), p(5,0)));
	    assert.equal(3, manhattanDistance(p(0,3), p(0,0)));
	});
    });
});
