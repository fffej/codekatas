"use strict";

var assert = require('assert');

var createBoard = function(rows) {
    return true;
};

describe('reversi', function() {
    it('should give no legal moves', function() {
	var board = createBoard(['BW','BW']);
	assert(board !== undefined);
    });
});
