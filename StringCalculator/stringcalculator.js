"use strict";

var assert = require('assert');

var add = function(str) {
    if (str.length === 0) return 0;

    return str|0;
};

describe('string calculator', function() {
    describe('add', function() {
	it('should return a 0 for empty string', function() {
	    assert.equal(0, add(''));
	});

	it('should return identity for a single number', function() {
	    assert.equal(1, add('1'));
	});
    });
});

