"use strict";

var assert = require('assert');

var add = function(s) {
    return 0;
};

describe('string calculator', function() {
    describe('add', function() {
	it('0 numbers returns an empty string', function() {
	    var result = add('');
	    assert.equal(0, result);
	});
    });
});
