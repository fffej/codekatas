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

	var createModel = function(predictionContext) {
	    return markov.buildModel(predictionContext, [
		'the',
		'quick',
		'brown',
		'fox',
		'the',
		'quick'
	    ]);
	};

	it('length of sequence', function() {
	    var run = createModel(2).generate(['the'],10);
	    assert.equal(10, run.length);
	});

	it('has appropriate first index', function() {
	    var run = createModel(2).generate(['the'],1);
	    assert.deepEqual(['the'],run);
	});

	it('generates words', function() {
	    var run = createModel(2).generate(['the'],2);
	    assert.deepEqual(['the','quick'],run);
	});

	it.only('generates words with wider window', function() {
	    var run = createModel(3).generate(['the'],3);
	    assert.deepEqual(['the','quick','brown'], run);
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
