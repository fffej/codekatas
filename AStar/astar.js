"use strict";

var assert = require('assert');

describe('a* search algorithm', function() {
    describe('costs', function() {
	it('flatlands cost 1', function() {
	    assert.equal(1, cost('.'));
	});
    });
});
