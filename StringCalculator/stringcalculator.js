"use strict";

var assert = require('assert');

var toNumber = function(x) { return x | 0; };

var addXY = function(x,y) { return x + y; };

var splitToNumbers = function(str) {
    return str.replace(/\n/,',').split(',');
};

var add = function(str) {
     return splitToNumbers(str).map(toNumber).reduce(addXY,0);
};

describe('string calculator', function() {
    it('should simply add', function() {
	assert.equal(1, add('1'));
    });

    it('should be identity for single numbers', function() {
	assert.equal(123, add('123'));
    });

    it('should add numbers separated by commas', function() {
	assert.equal(2, add('1,1'));
    });

    it('should treat newlines as commas', function() {
	assert.equal(2, add('1\n1'));
    });

    it('should handle mixed , and \' \'', function() {
	assert.equal(3, add('1,1\n1'));
    });
});
