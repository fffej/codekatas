"use strict";

var assert = require('assert');


var add = function(str) {
     return str.split(',').map(function(x) { return x | 0; }).reduce(function(x,y) { return x + y; },0);
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
});
