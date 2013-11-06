"use strict";

var assert = require('assert');

var statistics = function() {

};

describe('sequence processing', function() {
    it('should return the count', function() {
	assert.equal(0, statistics([]).count);
    });
});
