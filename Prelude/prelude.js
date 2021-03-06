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

var nth = function(list,n) {
    return n === 0 ? head(list) : nth(list.tail, n-1);
};

var map = function(list,f) {
    if (isNull(list)) return list;

    return new Cons(
	f(head(list)),
	map(tail(list),f)
    );
};

var filter = function(list,p) {
    if (isNull(list)) return list;

    if (p(head(list))) return new Cons(head(list), filter(tail(list),p));
    
    return filter(tail(list),p);
};

var isNull = function(list) {
    return !list;
};

var makeList = function() {
    if (arguments.length === 0) {
	return undefined;
    }

    var args = Array.prototype.slice.call(arguments);
    var head = args.shift();

    return new Cons(head, args.length === 0 ? undefined : makeList.apply(null,args));
};

describe('prelude', function() {
    describe('lists', function() {
	it('should make an empty list', function() {
	    assert.deepEqual(undefined, makeList());
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

	it('can map', function() {
	    var addOne = function(x) { return x + 1; };

	    assert.deepEqual(makeList(2,3,4), map(makeList(1,2,3), addOne));
	});

	it('can filter', function() {
	    var isOdd = function(x) { return x % 2 !== 0; };

	    assert.deepEqual(makeList(1,3,5), filter(makeList(1,2,3,4,5), isOdd));
	});

	it('should support null', function() {
	    assert(isNull(makeList()));
	    assert(!isNull(makeList(1)));
	});
    });
});
