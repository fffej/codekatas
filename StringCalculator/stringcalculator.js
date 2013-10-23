"use strict";

var assert = require('assert');

var isCustomDelimiter = function(str) {
    return str[0] === '/' && str[1] === '/';
};

var stripDelimiter = function(str) {
    return isCustomDelimiter(str) ? str.substr(4) : str;
};

var getDelimiter = function(str) {
    return isCustomDelimiter(str) ? str[2] : ',';
};

var add = function(str) {
    if (str.length === 0) return 0;
    str = str.replace(/\n/,',');

    var delim = getDelimiter(str);
    str = stripDelimiter(str);

    var sum = 0;
    var nums = str.split(delim);
    for (var i=0;i<nums.length;++i) {
	sum += nums[i] | 0;
    }

    return sum;
};

describe('string calculator', function() {
    describe('add', function() {
	it('should return a 0 for empty string', function() {
	    assert.equal(0, add(''));
	});

	it('should return identity for a single number', function() {
	    assert.equal(1, add('1'));
	});

	it('should return sum for numbers separated by ,', function() {
	    assert.equal(3, add('1,2'));
	});

	it('should handle numbers spparated by newlines', function() {
	    assert.equal(3, add('1\n2'));
	});

	it('should support different delimiters', function() {
	    assert.equal(3, add('//;\n1;2'));
	});
    });
});

