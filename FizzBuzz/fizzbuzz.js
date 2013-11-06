"use strict";

var assert = require('assert');

var fizzbuzz = function(i) {
    if (i % 3 === 0) return 'Fizz';
    if (i % 5 === 0) return 'Buzz';

    return i;
};

describe('fizz buzz', function() {
    it('numbers not divisible by 3 or 5 should be identity', function() {
	assert.equal(1, fizzbuzz(1));
	assert.equal(4, fizzbuzz(4));
    });

    it('numbers divisible by 3 should Fizz', function() {
	assert.equal('Fizz', fizzbuzz(3));
    });

    it('numbers divisible by 5 should Buzz', function() {
	assert.equal('Buzz', fizzbuzz(5));
    });
});
