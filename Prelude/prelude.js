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
	    var l = makeList(1);

	    assert.equal(l.head, 1);
	    assert.equal(undefined, l.tail);
	});
    });
});
