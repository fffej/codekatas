"use strict";

var assert = require('assert');

var add = function(str) {
    return 0;
};

describe('string calculator', function() {
    describe('add', function() {
	it('should return a 0 for empty string', function() {
	    assert.equal(0, add(''));
	});
    });
});

