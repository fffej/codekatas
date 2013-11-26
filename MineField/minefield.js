"use strict";

var assert = require('assert');

var produceMineField = function(s) {
    return s === '*' ? '*' : '0';
};

describe('mine field', function() {
    it('works for a single mine', function() {
	assert.equal('*', produceMineField('*'));
    });

    it('works for an empty field', function() {
	assert.equal('0', produceMineField('.'));
    });

    it('works for a 1x2 empty field', function() {
	assert.equal('0\n0\n', produceMineField('.\n.'));
    });
});
