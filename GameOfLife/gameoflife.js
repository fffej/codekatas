"use strict";
var assert = require('assert');

var CellState = {
  DEAD: 0,
  ALIVE: 1
};

var Cell = function(cellState) {
 
  var overOrUnderPopulated = function(n) {
    return n < 2 || n > 3;
  };

  this.isDead = function() { return cellState === CellState.DEAD; };
  this.isAlive = function() { return !this.isDead(); };

  this.nextState = function(numOfLiveNeighbours) {
    var next = cellState;

    if (this.isAlive() && overOrUnderPopulated(numOfLiveNeighbours)) {
      next = CellState.DEAD;
    } else if (this.isDead() && numOfLiveNeighbours === 3) {
      next = CellState.ALIVE;
    }

    return new Cell(next);
  }

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

  describe('infinite grid', function() {
    it('exists!', function() {
      assert(new InfiniteGrid());
    });
  });

  describe('cells', function() {
    it('cells exist', function() {
      assert(new Cell(CellState.DEAD));
      assert(new Cell(CellState.ALIVE));
    });

    describe('transitions', function() {

      var liveCell = function() { return new Cell(CellState.ALIVE); };
      var deadCell = function() { return new Cell(CellState.DEAD); };

      it('live to dead less than two neighbours', function() {
        assert(liveCell().nextState(1).isDead());
      });

      it('live to dead more than three neighbours', function() {
        assert(liveCell().nextState(4).isDead());
      });

      it('staying alive with two or three neighbours', function() {
        assert(liveCell().nextState(2).isAlive());
        assert(liveCell().nextState(3).isAlive());
      });

      it('dead cells become alive with three neighbours', function() {
        assert(deadCell().nextState(3).isAlive());
      });

      it('is immutable', function() {
        var liveCell = new Cell(CellState.ALIVE);
        var nextState = liveCell.nextState(1);
  
        assert(liveCell.isAlive());
        assert(nextState.isDead());
      });
    });
  });
});