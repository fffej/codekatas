"use strict";

var assert = require('assert');

var statistics = function(s) {
    var n = s.length;
    var isEmpty = n === 0;

    var defaults = {
	count: n, 
	maximum: isEmpty ? undefined : s[0],
	minimum: isEmpty ? undefined : s[0],
	average: isEmpty ? undefined : 0
    };

    return s.reduce(function(x,y) {
	x.minimum = Math.min(y,x.minimum);
	x.maximum = Math.max(y,x.maximum);
	x.average = (y / n) + x.average;
	return x;
    }, defaults);
};

describe('sequence processing', function() {
    it('should return the count', function() {
	assert.equal(0, statistics([]).count);
	assert.equal(3, statistics([1,2,3]).count);
    });

    it('avg, min and max should be undefined for empty lists', function() {
	var stats = statistics([]);

	assert.equal(undefined, stats.minimum);
	assert.equal(undefined, stats.maximum);
	assert.equal(undefined, stats.average);
    });

    it('min works as expected', function() {
	assert.equal(0, statistics([0]).minimum);	
	assert.equal(3, statistics([5,4,3]).minimum);
    });

    it('max works as expected', function() {
	assert.equal(0, statistics([0]).maximum);
	assert.equal(5, statistics([5,4,3]).maximum);
    });

    it('average works as expected', function() {
	assert.equal(0, statistics([0]).average);
	assert.equal(3, statistics([2,4]).average);
    });
});
