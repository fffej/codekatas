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

var splitCustomDelimiter = function(s) {
    var splitChar = s[2];
    var rest = s.split('\n')[1];
    return rest.split(splitChar);
};

var split = function(s) {
    if (s.indexOf('//') === 0) 
	return splitCustomDelimiter(s);

    return s.split(/,|\n/);
};

var ignoreNumbers = function(arr) {
    return arr.filter(function(x) { return x <= 1000; });
};

var validate = function(arr) {
    var illegal = arr.filter(function(x) { return x < 0; });

    if (illegal.length !== 0)
	throw Exception(illegal);

    return arr;
};

var add = function(s) {
    return sum(ignoreNumbers(validate(coerceToNums(split(s)))));
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
	assert.equal(5, add('//;\n3;2'));
    });

    it('ignores numbers over 1000', function() {
	assert.equal(2, add('1001,2'));
    });

    it('throws on negative numbers', function() {
	assert.throws(function() {
	    add('-1');
	});
    });
});
