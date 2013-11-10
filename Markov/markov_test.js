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
		'fox'
	    ]);
	});

	it('should provide entries', function() {
	    assert.equal(4, model.entryCount());
	});
    });
});
