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

var sumValues = function(str, delim) {
    var sum = 0;
    var nums = str.split(delim);

    var invalidValues = [];

    for (var i=0;i<nums.length;++i) {
	var num = nums[i]|0;

	if (num < 0) invalidValues.push(num);

	sum += num;
    }

    if (invalidValues.length !== 0)
	throw new Error('negatives not allowed: ' + invalidValues);

    return sum;
};

var stripInvalidCharacters = function(str) {
    return str.replace(/\n/,',');
};

var add = function(str) {
    str = stripInvalidCharacters(str);
    var delim = getDelimiter(str);
    str = stripDelimiter(str);

    return sumValues(str,delim);
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

	it('should not support negative numbers', function() {
	    assert.throws(function() {
		add('-1');
	    }, function(err) { 
		assert(err.message.indexOf('-1') !== -1);
		return true;
	    });
	});

	it('should display all negative numbers', function() {
	    assert.throws(function() { add('-1,-2'); },
			  function(err) {
			      assert(err.message.indexOf('-1') !== -1);
			      assert(err.message.indexOf('-2') !== -1);
			      return true;
			  });
	});
    });
});

