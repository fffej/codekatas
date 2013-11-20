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

var And = function(input1,input2,output) {
    input1.onChange(function(s) {
	output.setSignal(s && input2.getSignal());
    });

    input2.onChange(function(s) {
	output.setSignal(s && input1.getSignal());
    });
};

var Or = function(input1,input2,output) {
    input1.onChange(function(s) {
	output.setSignal(s || input2.getSignal());
    });

    input2.onChange(function(s) {
	output.setSignal(s || input1.getSignal());
    });
};

var makeWire = function() {
    return new Wire();
};

var makeInverter = function(input,output) {
    return new Inverter(input,output);
};

var makeAnd = function(input1,input2,output) {
    return new And(input1,input2,output);
};

var makeOr = function(input1,input2,output) {
    return new Or(input1,input2,output);
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

	input.setSignal(true);
	assert(!output.getSignal());
    });

    it('has an and gate', function() {
	var input1 = makeWire(),
	    input2 = makeWire(),
	    output = makeWire();

	var andGate = makeAnd(input1, input2, output);

	assert(!output.getSignal());
	
	input1.setSignal(true);
	input2.setSignal(true);

	assert(output.getSignal());
    });

    it('has an or gate', function() {
	var input1 = makeWire(),
	    input2 = makeWire(),
	    output = makeWire();

	var orGate = makeOr(input1, input2, output);

	assert(!output.getSignal());

	input1.setSignal(true);
	assert(output.getSignal());

	input1.setSignal(false);
	assert(!output.getSignal());

	input2.setSignal(true);
	assert(output.getSignal());
    });
});
