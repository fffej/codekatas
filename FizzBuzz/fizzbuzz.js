"use strict";

var assert = require('assert');

var fizzbuzz = function(i) {
    return 1;
};

describe('fizz buzz', function() {
    it('numbers not divisible by 3 or 5 should be identity', function() {
	assert.equal(1, fizzbuzz(1));
    });
});
