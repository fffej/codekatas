"use strict";
var assert = require('assert');

var Cell = {
  DEAD: 0,
  ALIVE: 1
};

describe('game of life', function() {
  describe('states', function() {
    it('dead or alive', function() {
      assert(Cell.DEAD !== undefined);
      assert(Cell.ALIVE);
      assert(Cell.DEAD !== Cell.ALIVE);
    });
  });
});