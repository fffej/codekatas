"use strict";
var assert = require('assert');

var LIVE = 1;
var DEAD  = 0;

var cellState = function(currentCellState, numberOfAliveNeighbours) {
  var twoOrThreeNeighbours = numberOfAliveNeighbours === 2 ||
                             numberOfAliveNeighbours === 3;

  if (numberOfAliveNeighbours < 2 || numberOfAliveNeighbours > 3) {
    return DEAD;
  } 
  else if (currentCellState === LIVE && twoOrThreeNeighbours) { 
    return LIVE;
  }
};

describe('Game of life', function() {
  describe('cell', function() {
    it('fewer than two neighbours dies', function() {
      assert.equal(DEAD, cellState(LIVE,1));
    });

    it('more than 3 alive cells dies', function() {
      assert.equal(DEAD, cellState(LIVE,4));
    });

    it('live cells with 2 or 3 neighbours live on', function() {
      assert.equal(LIVE, cellState(LIVE,2));
      assert.equal(LIVE, cellState(LIVE,3));
    });
  });
});
