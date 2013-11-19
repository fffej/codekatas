"use strict";

var assert = require('assert');

var cost = function(c) {
    return 1;
};

describe('a* search algorithm', function() {
    describe('costs', function() {
	it('flatlands cost 1', function() {
	    assert.equal(1, cost('.'));
	    assert.equal(1, cost('@'));
	    assert.equal(1, cost('X'));
	});
    });
});
