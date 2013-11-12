"use strict";

var assert = require('assert');

var Board = function(rows) {

    var opposite = function(turn) {
	return turn === 'B' ? 'W' : 'B';
    };

    this.isValidMove = function(i,j, turn) {
	if (rows[i][j] !== '-')
	    return false;

	// Walk left
	var w = j-1;
	while (w >= 0 && rows[i][w] === opposite(turn))
	    w--;

	if (w >= 0 && w !== j-1) {
	    return (rows[i][w] === turn);
	}

	return false;
    };

    this.legalMoves = function(turn) {

	var moves = [];

	for (var i=0;i<rows.length;++i)
	    for (var j=0;j<rows[i].length;++j)
		if (this.isValidMove(i,j,turn)) 
		    moves.push({row: i, col: j});

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
				 'BBB']);

	assert.equal(1, board.legalMoves('B').length);
    });

    it('should not return any valid moves', function() {
	var board = createBoard(['BW-',
				 'BBB',
				 'BBB']);

	assert.equal(0, board.legalMoves('W').length);
    });
});
