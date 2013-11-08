"use strict";

var assert = require('assert');

var Cons = function(head,tail) {
    this.head = head;
    this.tail = tail;
    return this;
};

var makeList = function() {
    return new Cons(arguments[0], arguments[1]);
};

var List = {
    empty: new Cons()
};

describe('prelude', function() {
    describe('lists', function() {
	it('should make an empty list', function() {
	    assert.deepEqual(List.empty, makeList());
	});

	it('should make a cons cell', function() {
	    var list = makeList(1);

	    assert.equal(1, list.head);
	    assert.equal(undefined, list.tail);
	});

	it('should handle multiple elements', function() {
	    var list = makeList(1,2,3);

	    assert.equal(1,         list.head);
	    assert.equal(2,         list.tail.head);
	    assert.equal(3,         list.tail.tail.head);
	    assert.equal(undefined, list.tail.tail.tail);
	});
    });
});
