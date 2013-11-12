"use strict";

var assert = require('assert');

var Board = function(rows) {
    return this;
};

var createBoard = function(rows) {
    return new Board(rows);
};

describe('reversi', function() {
    it('should give no legal moves', function() {
	var board = createBoard(['BW','BW']);
	assert(board !== undefined);
    });
});
