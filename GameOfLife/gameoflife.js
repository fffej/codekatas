"use strict";
var assert = require('assert');

var ALIVE = 1;
var DEAD  = 0;

var cellState = function(currentCellState, numberOfAliveNeighbours) {
  if (numberOfAliveNeighbours < 2) {
    return DEAD;
  }
};

describe('Game of life', function() {
  describe('cell', function() {
    it('fewer than two neighbours dies', function() {
      assert.equal(DEAD, cellState(ALIVE,1));
    });

    it('more than 3 alive cells dies', function() {
      assert.equal(DEAD, cellState(ALIVE,4));
    });
  });
});
