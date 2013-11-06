"use strict";

var assert = require('assert');

describe('sequence processing', function() {
    it('should return the count', function() {
	assert.equal(0, statistics([]).count);
    });
});
