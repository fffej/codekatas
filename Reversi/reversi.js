"use strict";

var assert = require('assert');

var Board = function(rows) {
    this.legalMoves = function(turn) {

	var moves = [];

	for (var i=0;i<rows.length;++i) {
	    for (var j=0;j<rows[i].length;++j) {
		if (rows[i][j] === '-') {
		    moves.push({row: i, col: j});
		}
	    }
	}

	return moves;
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

    it('should not return any valid moves', function() {
	var board = createBoard(['BW-',
				 'BBB',
				 'BBB'], 'W');

	assert.equal(0, board.legalMoves().length);
    });
});
