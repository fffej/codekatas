"use strict";

var assert = require('assert');

var add = function(str) {
    return 1;
};

describe('string calculator', function() {
    it('should simply add', function() {
	assert.equal(1, add('1'));
    });
});
