"use strict";

var assert = require('assert');

describe('prelude', function() {
    describe('map', function() {
	it('should preserve []', function() {
	    assert.deepEqual([], map([], id));
	});
    });
});
