"use strict";

var assert = require('assert');

var statistics = function(s) {

    var minFn = function(x,y) { return Math.min(x,y); };
    var maxFn = function(x,y) { return Math.max(x,y); };
    var avgFn = function(x,y) { return y + (x / s.length); };

    return {
	count: s.length,
	maximum: s.length === 0 ? undefined : s.reduce(maxFn, s[0]),
	minimum: s.length === 0 ? undefined : s.reduce(minFn, s[0]),
	average: s.length === 0 ? undefined : s.reduce(avgFn, s[0])
    };
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
    });
});
