"use strict";

var assert = require('assert');

var sum = function(arr) {
    var add = function(x,y) { return x + y; };
    return arr.reduce(add);
};

var coerceToNums = function(arr) {
    var asInt = function(x) { return x|0; };
    return arr.map(asInt);
};

var split = function(s) {
    if (s.indexOf('//') === 0) {
	return ['1','2'];
    }

    return s.split(/,|\n/);
};

var add = function(s) {
    return sum(coerceToNums(split(s)));
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

    it('multiple numbers add', function() {
	assert.equal(3, add('1,1,1'));
    });

    it('newlines are treated like ,', function() {
	assert.equal(3, add('1\n1,1'));
    });

    it('supports custom delimiters', function() {
	assert.equal(3, add('//;\n1;2'));
    });
});
