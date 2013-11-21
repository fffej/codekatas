"use strict";

var assert = require('assert');
var Rx = require('rx');
var EventEmitter = require('events').EventEmitter;

var eventEmitter = new EventEmitter();
var source = Rx.Observable.fromEvent(eventEmitter, 'signalChange');

var Wire = function(name) {
    
    var signal = false;

    var observers = [];

    this.getSignal = function() {
	return signal;
    };

    this.setSignal = function(s) {
	signal = !!s;
	eventEmitter.emit('signalChange', this);
    };

    return this;
};

var isWire = function(x) {
    return function(y) {
	return x === y;
    };
};

var isWires = function(arr) {
    var n = arr.length;

    return function(y) {
	for (var i=0;i<n;++i)
	    if (arr[i] === y) return true;

	return false;
    };
};

var Inverter = function(input,output) {

    source.where(isWire(input)).subscribe(function(wire) {
	output.setSignal(!wire.getSignal());
    });

    input.setSignal(input.getSignal());

    return this;
};

var And = function(input1,input2,output) {
    source.where(isWires([input1,input2])).subscribe(function(wire) {
	output.setSignal(input1.getSignal() && input2.getSignal());
    });
};

var Or = function(input1,input2,output) {
    source.subscribe(function(wire) {
	if (wire === input1 || wire === input2) 
	    output.setSignal(input1.getSignal() || input2.getSignal());
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

exports.makeWire = makeWire;
exports.makeInverter = makeInverter;
exports.makeAnd = makeAnd;
exports.makeOr = makeOr;
