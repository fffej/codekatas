"use strict";

var assert = require('assert');

var isFizz = function(n) { return n % 3 === 0; };

var isBuzz = function(n) { return n % 5 === 0; };

var fizzbuzz = function(i) {
    if (isFizz(i)) return 'Fizz';
    if (isBuzz(i)) return 'Buzz';

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

    it('numbers divisible by 3 and 5 should fizz buzz', function() {
	assert.equal('FizzBuzz', fizzbuzz(3*5));
    });
});
