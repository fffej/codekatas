"use strict";

var assert = require('assert');
var markov = require('./markov.js');

describe('Markov chain', function() {
    describe('building model', function() {

	var model = null;
	
	beforeEach(function() {
	    model = markov.buildModel([
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
});
