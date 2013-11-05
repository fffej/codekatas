"use strict";

var assert = require('assert');

var add = function(str) {
    var parts = str.split(',');
    var sum = 0;
    for (var i=0;i<parts.length;++i) {
	sum += (parts[i]|0);
    }
    return sum;
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
