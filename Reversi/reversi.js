"use strict";

var assert = require('assert');

var Board = function(rows) {
    this.legalMoves = function() {
	return [];
    };

    return this;
};

var createBoard = function(rows) {
    return new Board(rows);
};

describe('reversi', function() {
    it('should create a board', function() {
	var board = createBoard(['BW','BW']);
	assert(board !== undefined);
    });

    it('should return no valid moves', function() {
	var board = createBoard(['BW',
				 'BW']);

	assert.equal(0, board.legalMoves().length);
    });
});
