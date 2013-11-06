"use strict";

var assert = require('assert');

var statistics = function(s) {
    return {
	count: s.length
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
});
