"use strict";

var assert = require('assert');

var List = {
    empty: new Cons()
};

describe('prelude', function() {
    describe('lists', function() {
	it('should make an empty list', function() {
	    assert.equal(List.empty, makeList());
	});
    });
});
