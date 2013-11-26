"use strict";

var assert = require('assert');

var surroundingMines = function() {
    
    var isMine = function(r) {
	return r === '*' ? '*' : '0';
    };

    return function(x) {
	return x.split('').map(isMine).join('');
    };
};

var produceMineField = function(s) {
    var rows = s.split('\n');
    return rows.map(surroundingMines()).join('\n');
};

describe('mine field', function() {
    it('works for a single mine', function() {
	assert.equal('*', produceMineField('*'));
    });

    it('works for an empty field', function() {
	assert.equal('0', produceMineField('.'));
    });

    it('works for a 1x2 empty field', function() {
	assert.equal('0\n0', produceMineField('.\n.'));
    });

    it('works for a 2x1 empty field', function() {
	assert.equal('00\n', produceMineField('..\n'));
    });
});
