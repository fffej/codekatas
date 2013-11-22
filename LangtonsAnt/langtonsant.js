"use strict";
var assert = require('assert');

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

var makeSquare = function() {
    return {};
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
    });
});
