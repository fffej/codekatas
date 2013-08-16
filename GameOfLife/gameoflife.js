"use strict";
var assert = require('assert');

var CellState = {
  DEAD: 0,
  ALIVE: 1
};

var Cell = function(cellState) {
 
  this.isDead = function() { return cellState === CellState.DEAD; };
  this.isAlive = function() { return !this.isDead(); };

  this.nextState = function(numOfLiveNeighbours) {
    if (this.isAlive() && numOfLiveNeighbours < 2) {
      cellState = CellState.DEAD;
    }

    return this;
  };

  return this;
};

describe('game of life', function() {
  describe('states', function() {
    it('dead or alive', function() {
      assert(CellState.DEAD !== undefined);
      assert(CellState.ALIVE);
      assert(CellState.DEAD !== CellState.ALIVE);
    });
  });

  describe('cells', function() {
    it('cells exist', function() {
      assert(new Cell(CellState.DEAD));
      assert(new Cell(CellState.ALIVE));
    });

    describe('transitions', function() {

      var liveCell = function() { return new Cell(CellState.ALIVE); };

      it('live to dead less than two neighbours', function() {
        assert(liveCell().nextState(1).isDead());
      });

      it('live to dead more than three neighbours', function() {
        assert(liveCell().nextState(4).isDead());
      });
    });
  });
});