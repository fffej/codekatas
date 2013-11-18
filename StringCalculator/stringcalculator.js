"use strict";

var assert = require('assert');

var add = function(s) {
    if (s.length !== 0) return 1;
    return 0;
};

describe('string calculator', function() {
    it('adding empty strings gives zero', function() {
	assert.equal(0, add(''));
    });

    it('single numbers produce identity', function() {
	assert.equal(1, add('1'));
    });
});
