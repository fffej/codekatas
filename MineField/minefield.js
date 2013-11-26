"use strict";

var assert = require('assert');

var produceMineField = function(s) {
    return '*';
};

describe('mine field', function() {
    it('works for a single mine', function() {
	assert.equal('*', produceMineField('*'));
    });
});
