"use strict";
var assert = require('assert');

var Color = {
    White: 0
};

var Ant = function() {
    
    var position = {x: 0, y: 0};

    this.position = function() {
	return position;
    };

    return this;
};


var makeAnt = function() {
    return new Ant();
};

var Square = function() {

    var color = Color.White;

    this.color = function() { 
	return color;
    };

    this.flip = function() {
	color = (color == Color.White ? Color.Black : Color.White);
    };

    return this;
};

var makeSquare = function() {
    return new Square();
};

describe('langton\'s ant', function() {
    describe('ant', function() {
	it('exists', function() {
	    assert(makeAnt());
	});

	it('has a location', function() {
	    var ant = makeAnt();
	    assert.equal(0, ant.position().x);
	    assert.equal(0, ant.position().y);
	});
    });

    describe('square', function() {
	it('exists', function() {
	    assert(makeSquare());
	});

	it('initially white', function() {
	    assert.equal(Color.White, makeSquare().color());
	});

	it('flips', function() {
	    var sq = makeSquare();
	    sq.flip();
	    assert.equal(Color.Black, sq.color());
	});
    });
});
