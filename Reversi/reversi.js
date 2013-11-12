"use strict";

var assert = require('assert');

var Board = function(rows) {
    this.legalMoves = function(turn) {
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

	assert.equal(0, board.legalMoves('B').length);
    });

    it('should return the single valid move', function() {
	var board = createBoard(['BW-',
				 'BBB',
				 'BBB'], 'B');

	assert.equal(1, board.legalMoves().length);
    });
});
