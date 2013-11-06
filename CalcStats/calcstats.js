"use strict";

var assert = require('assert');

var statistics = function(s) {

    var minFn = function(x,y) { return Math.min(x,y); };

    return {
	count: s.length,
	minimum: s.length === 0 ? undefined : s.reduce(minFn, s[0])
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

    it('min works as expected on lists', function() {
	assert.equal(0, statistics([0]).minimum);	
	assert.equal(3, statistics([5,4,3]).minimum);
    });

    it('max works as expected', function() {
	assert.equal(0, statistics([0]).maximum);
    });

});
