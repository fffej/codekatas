"use strict";

var assert = require('assert');

var cost = function(c) {
    if (c === '^') return 3;

    if (c === '*') return 2;

    return 1;
};

var walkable = function(c) {
    return !(c === undefined || c === '~');
};

var p = function(x,y) {
    return {
	x: x,
	y: y
    };
};

var ptEqual = function(p1,p2) {
    return p1.x === p2.x && p1.y === p2.y;
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

    this.at = function(pt) {
	if (this.rows[pt.y])
	    return this.rows[pt.y][pt.x];
    };

    var surroundings = function(pt) {
	var left = p(pt.x-1,pt.y);
	var right = p(pt.x+1,pt.y);
	var up = p(pt.x, pt.y-1);
	var down = p(pt.x, pt.y+1);

	return [left,right,up,down];
    };

    this.moves = function(pt) {
	var choices = [];
	var deltas = surroundings(pt);

	for (var i=0;i<deltas.length;++i) 
	    if (walkable(this.at(deltas[i]))) 
		choices.push(deltas[i]);

	return choices;
    };

    this.cost = function(pt) {
	return this.costSoFar + cost(this.at(pt));
    };

    this.solve = function(callback) {
	// first move is to the start destination
	var start = this.start();
	callback(start);

	var choices = this.moves(start);
	for (var i=0;i<choices.length;++i)
	    if (ptEqual(choices[i],this.goal()))
		callback(choices[i]);

    };

    this.costSoFar = 0;

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

	it('finds choices', function() {
	    var map = load(defaultMap);
	    
	    var choices = map.moves(map.start());

	    assert.equal(2, choices.length);
	});

	it('can cost choices', function() {
	    var map = load(defaultMap);
	    var choices = map.moves(map.start());

	    assert.equal(map.cost(choices[0]), map.cost(choices[1]));
	    assert.equal(1, map.cost(choices[0]));
	});

	it('can cost variable choices', function() {
	    var map = load('@*\n.X');
	    var choices = map.moves(map.start());

	    assert(map.cost(choices[0]) !== map.cost(choices[1]));
	});

	it('has a cost-so-far metric', function() {
	    var map = load(defaultMap);
	    assert.equal(0, map.costSoFar);
	});

	it('uses cost so far when costing choices', function() {
	    var map = load('@*\n.X');
	    map.costSoFar = 100001;

	    var choices = map.moves(map.start());
	    
	    assert(map.cost(choices[0]) > map.costSoFar);
	    assert(map.cost(choices[1]) > map.costSoFar);
	});

	it('makes the correct move', function() {
	    var map = load('@*\nX.');
	    var moves = [];

	    map.solve(function(x) { moves.push(x); });

	    assert.equal(2, moves.length);
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

	it('non-walkable', function() {
	    assert(!walkable(undefined));
	    assert(!walkable('~'));
	});
    });

    describe('Manhattan distance', function() {
	it('works in a straight line', function() {
	    assert.equal(5, manhattanDistance(p(0,0), p(5,0)));
	    assert.equal(3, manhattanDistance(p(0,3), p(0,0)));
	});
    });
});
