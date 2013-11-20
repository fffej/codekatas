"use strict";

var assert = require('assert');

var Wire = function() {
    
    var signal = false;

    this.getSignal = function() {
	return signal;
    };

    this.setSignal = function(s) {
	signal = !!s;
    };

    return this;
};

var makeWire = function() {
    return new Wire();
};

describe('circuit simulator', function() {
    it('wires have a signal', function() {
	var w = makeWire();
	assert.equal(false, w.getSignal());

	w.setSignal(true);
	assert.equal(true, w.getSignal());
    });
});
