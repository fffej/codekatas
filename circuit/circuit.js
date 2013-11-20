"use strict";

var assert = require('assert');

describe('circuit simulator', function() {
    it('wires have a signal', function() {
	var w = makeWire();
	assert.equal(false, w.getSignal());

	w.setSignal(true);
	assert.equal(true, w.getSignal());
    });
});
