"use strict";
var assert = require('assert');

var makeAnt = function() {
    return {};
};

describe('langton\'s ant', function() {
    describe('ant', function() {
	it('exists', function() {
	    assert(makeAnt());
	});

	it('has a location', function() {
	    var ant = makeAnt();
	    assert.equal(0, ant.getPos().x);
	    assert.equal(0, ant.getPos().y);
	});
    });
});
