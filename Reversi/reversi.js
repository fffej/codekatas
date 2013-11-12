"use strict";

var assert = require('assert');

describe('reversi', function() {
    it('should give no legal moves', function() {
	var board = createBoard(['BW','BW']);
	assert.equal(0,board.moves().length);
    });
});
