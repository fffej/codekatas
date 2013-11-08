"use strict";

var assert = require('assert');

var Cons = function(head,tail) {
    this.head = head;
    this.tail = tail;
    return this;
};

var makeList = function() {
    return new Cons();
};

var List = {
    empty: new Cons()
};

describe('prelude', function() {
    describe('lists', function() {
	it('should make an empty list', function() {
	    assert.deepEqual(List.empty, makeList());
	});
    });
});
