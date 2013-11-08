"use strict";

var assert = require('assert');

var id = function(x) { return x; };

var map = function(arr,f) {
    return arr;
};

describe('prelude', function() {
    describe('map', function() {
	it('should preserve []', function() {
	    assert.deepEqual([], map([], id));
	});

	it('should preserve array', function() {
	    assert.deepEqual([1,2,3], map([1,2,3], id));
	});

	it('should apply function', function() {
	    assert.deepEqual([1,2,3], map([0,1,2], function(x) { return x + 1; }));
	});
    });
});
