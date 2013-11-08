"use strict";

var assert = require('assert');


var Cons = function(head,tail) {
    this.head = head;
    this.tail = tail;
    return this;
};

var head = function(l) {
    return l.head;
};

var tail = function(l) {
    return l.tail;
};

var makeList = function() {
    if (arguments.length === 0) {
	return new Cons(undefined,undefined);
    }

    var args = Array.prototype.slice.call(arguments);
    var head = args.shift();

    return new Cons(head, args.length === 0 ? undefined : makeList.apply(null,args));
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

	it('should have head and tail functions', function() {
	    assert.equal(1, head(makeList(1,2,3)));
	    assert.equal(2, head(tail(makeList(1,2,3))));
	});

	it('should be possible get the nth element', function() {
	    assert.equal(2, nth(makeList(1,2,3),1));
	});
    });
});
