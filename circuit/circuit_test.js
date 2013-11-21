"use strict";

var assert = require('assert');
var circuit = require('./circuit.js');

describe('circuit simulator', function() {
    it('wires have a signal', function() {
	var w = circuit.makeWire();
	assert.equal(false, w.getSignal());

	w.setSignal(true);
	assert.equal(true, w.getSignal());
    });

    it.only('has an inverter', function() {
	var input = circuit.makeWire();
	var output = circuit.makeWire();

	var inverter = circuit.makeInverter(input,output);

	assert(output.getSignal());

	input.setSignal(true);
	assert(!output.getSignal());
    });

    it('has an and gate', function() {
	var input1 = circuit.makeWire(),
	    input2 = circuit.makeWire(),
	    output = circuit.makeWire();

	var andGate = circuit.makeAnd(input1, input2, output);

	assert(!output.getSignal());
	
	input1.setSignal(true);
	input2.setSignal(true);

	assert(output.getSignal());
    });

    it('has an or gate', function() {
	var input1 = circuit.makeWire(),
	    input2 = circuit.makeWire(),
	    output = circuit.makeWire();

	var orGate = circuit.makeOr(input1, input2, output);

	assert(!output.getSignal());

	input1.setSignal(true);
	assert(output.getSignal());

	input1.setSignal(false);
	assert(!output.getSignal());

	input2.setSignal(true);
	assert(output.getSignal());
    });
});
