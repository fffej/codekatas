"use strict";
var assert = require('assert');

var CellState = {
  DEAD: 0,
  ALIVE: 1
};

describe('game of life', function() {
  describe('states', function() {
    it('dead or alive', function() {
      assert(CellState.DEAD !== undefined);
      assert(CellState.ALIVE);
      assert(CellState.DEAD !== CellState.ALIVE);
    });
  });
});