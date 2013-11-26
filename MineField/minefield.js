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
	assert.equal('*\n', produceMineField('*\n'));
    });

    it('works for an empty field', function() {
	assert.equal('0\n', produceMineField('.\n'));
    });

    it('works for a 1x2 empty field', function() {
	assert.equal('0\n0\n', produceMineField('.\n.\n'));
    });

    it('single mine 1x2 field', function() {
	assert.equal('1\n*\n', produceMineField('.\n*\n'));
    });

    it('works for a 2x1 empty field', function() {
	assert.equal('00\n', produceMineField('..\n'));
    });

    it('works for a 2x3 field', function() {
	assert.equal('00\n00\n00\n', produceMineField('..\n..\n..\n'));
    });
});
