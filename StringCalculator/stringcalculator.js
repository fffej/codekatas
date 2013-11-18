"use strict";

var assert = require('assert');

var add = function(s) {
    var nums = s.split(',');
    if (nums.length !== 1) return (nums[0]|0) + (nums[1]|0);

    return s|0;
};

describe('string calculator', function() {
    it('adding empty strings gives zero', function() {
	assert.equal(0, add(''));
    });

    it('single numbers produce identity', function() {
	assert.equal(1, add('1'));
	assert.equal(3, add('3'));
    });

    it('two numbers add', function() {
	assert.equal(2, add('1,1'));
	assert.equal(3, add('1,2'));
    });
});
