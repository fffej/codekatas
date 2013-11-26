"use strict";

var assert = require('assert');

var surroundingMines = function(rows) {
    
    var mineCount = function(i,j) {
	assert(i !== undefined)
	assert(j !== undefined)

	var count = 0;

	for (var xd=-1;xd<=1;xd++) 
	    for (var yd=-1;yd<=1;yd++) {
		if (rows[xd+i] && rows[xd+i][yd+j] === '*')
		    count++;
	    }
	return count;
    };

    return function(x,i) {

	var isMine = function(r,j) {
	    return r === '*' ? '*' : mineCount(i,j);
	};


	return x.split('').map(isMine).join('');
    };
};

var produceMineField = function(s) {
    var rows = s.split('\n');
    return rows.map(surroundingMines(rows)).join('\n');
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
