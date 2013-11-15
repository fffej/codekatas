"use strict";

var assert = require('assert');

var convertToInt = function(s) {
    return s.map(function(x) { return x|0; });
};

var sum = function(s) {
    return s.reduce(function(x,y) { return x + y; });
};

var splitUp = function(s) {
    return s.split(',');
};

var add = function(s) {
    var tokens = splitUp(s);
    var asNumbers = convertToInt(tokens);

    return sum(asNumbers);
};

describe('string calculator', function() {
    describe('add', function() {
	it('0 numbers returns an empty string', function() {
	    var result = add('');
	    assert.equal(0, result);
	});

	it('single number returns the value', function() {
	    var result = add('1');
	    assert.equal(1, result);
	});

	it('should add 2 numbers', function() {
	    var result = add('1,1');
	    assert.equal(2, result);
	});
    });
});
