"use strict";

var assert = require('assert');
var markov = require('./markov.js');

describe('Markov chain', function() {
    describe('building model', function() {

	var model = null;
	
	beforeEach(function() {
	    model = markov.buildModel(2, [
		'the',
		'quick',
		'brown',
		'fox',
		'the',
		'quick'
	    ]);
	});

	it('should provide entries', function() {
	    assert.equal(6, model.entryCount());
	});

	it('should provide frequency analysis', function() {
	    assert.equal(2, model.frequencyOf('the','quick'));
	    assert.equal(1, model.frequencyOf('brown','fox'));
	});
    });

    describe('generating data', function() {

	var model = null;

	beforeEach(function() {
	    model = markov.buildModel(2, [
		'the',
		'quick',
		'brown',
		'fox',
		'the',
		'quick'
	    ]);
	});

	it('length of sequence', function() {
	    var run = model.generate(0,10,12345);
	    assert.equal(10, run.length);
	});
    });

    describe('larger prediction context', function() {
	it('should count the frequency of n', function() {
	    var model = markov.buildModel(3, ['a','b','c','d','e']);
	    assert.equal(1, model.frequencyOf('a','b','c'));
	});

	it('should have a wide window', function() {
	    var model = markov.buildModel(4, ['a','b','c','d','e']);
	    assert.equal(1, model.frequencyOf('a','b','c','d'));
	});
    });
});
