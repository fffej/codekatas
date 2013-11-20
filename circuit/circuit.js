"use strict";

var assert = require('assert');

var Wire = function() {
    
    var signal = false;

    var observers = [];

    this.getSignal = function() {
	return signal;
    };

    this.setSignal = function(s) {
	var n = observers.length;
	signal = !!s;
	for (var i=0;i<n;++i) 
	    observers[i](signal);
    };

    this.onChange = function(f) {
	observers.push(f);
	f(signal);
    };

    return this;
};

var Inverter = function(input,output) {

    input.onChange(function(signal) {
	output.setSignal(!signal);
    });

    return this;
};

var makeWire = function() {
    return new Wire();
};

var makeInverter = function(input,output) {
    return new Inverter(input,output);
};

describe('circuit simulator', function() {
    it('wires have a signal', function() {
	var w = makeWire();
	assert.equal(false, w.getSignal());

	w.setSignal(true);
	assert.equal(true, w.getSignal());
    });

    it('has an inverter', function() {
	var input = makeWire();
	var output = makeWire();

	var inverter = makeInverter(input,output);

	assert(output.getSignal());

	input.setSignal(false);
	assert(!output.getSignal());
    });
});
