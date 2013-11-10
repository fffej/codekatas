"use strict";

var assert = require('assert');
var markov = require('./markov.js');

describe('Markov chain', function() {
    describe('building model', function() {
	it('should provide entries', function() {
	    var model = markov.buildModel([
		'the',
		'quick',
		'brown',
		'fox'
	    ]);

	    assert.equal(4, model.entryCount());
	});
    });
});
