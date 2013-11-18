"use strict";

var assert = require('assert');

var add = function() {
    return 0;
};

describe('string calculator', function() {
    it('adding empty strings gives zero', function() {
	assert.equal(0, add(''));
    });
});
