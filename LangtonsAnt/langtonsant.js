"use strict";
var assert = require('assert');

var Color = {
    White: 0
};

var Orientation = {
    Up: 0,
    Left: 1,
    Right: 2
};

var Ant = function() {
    
    var position = {x: 0, y: 0};
    var orientation = Orientation.Up;

    this.position = function() {
	return position;
    };

    this.orientation = function() {
	return orientation;
    };

    this.left = function() {
	switch (orientation) {
	case Orientation.Up:
	    orientation = Orientation.Left;
	    break;
	case Orientation.Left:
	    orientation = Orientation.Down;
	    break;
	case Orientation.Down:
	    orientation = Orientation.Right;
	    break;
	}
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

	it('orientation up', function() {
	    var ant = makeAnt();
	    assert.equal(Orientation.Up, ant.orientation());
	});

	it('turns left', function() {
	    var ant = makeAnt();
	    ant.left();
	    assert.equal(Orientation.Left, ant.orientation());

	    ant.left();
	    assert.equal(Orientation.Down, ant.orientation());

	    ant.left();
	    assert.equal(Orientation.Right, ant.orientation());
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
