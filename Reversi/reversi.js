"use strict";

var assert = require('assert');

var Board = function(rows) {

    var opposite = function(turn) {
	return turn === 'B' ? 'W' : 'B';
    };

    var isValidMoveLeft = function(i,j,turn) {
	var w = j-1;
	while (w >= 0 && rows[i][w] === opposite(turn))
	    w--;

	if (w >= 0 && w !== j-1)
	    return (rows[i][w] === turn);
	else
	    return false;
    };

    var isValidMoveRight = function(i,j,turn) {
	var w = j+1;
	while (w < rows[i].length && rows[i][w] === opposite(turn))
	    w++;

	if (w < rows[i].length && w !== j+1)
	    return (rows[i][w] === turn);
	else
	    return false;
    };


    this.isValidMove = function(i,j, turn) {
	if (rows[i][j] !== '-')
	    return false;

	return isValidMoveLeft(i,j,turn) ||
	       isValidMoveRight(i,j,turn);
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

    it('should return the single valid move (left)', function() {
	var board = createBoard(['BW-',
				 'BBB',
				 'BBB']);

	assert.equal(1, board.legalMoves('B').length);
    });

    it('should not return any valid moves (left)', function() {
	var board = createBoard(['BW-',
				 'BBB',
				 'BBB']);

	assert.equal(0, board.legalMoves('W').length);
    });

    it('should return the single valid move (right)', function() {
	var board = createBoard(['-WB',
				 'BBB',
				 'BBB']);

	assert.equal(1, board.legalMoves('B').length);
    });

    it('should not return any valid moves (right)', function() {
	var board = createBoard(['-WB',
				 'BBB',
				 'BBB']);

	assert.equal(0, board.legalMoves('W').length);
    });

});
