"use strict";
var assert = require('assert');

describe('game of life', function() {
  describe('states', function() {
    it('dead or alive', function() {
      assert(Cell.DEAD);
      assert(Cell.ALIVE);
      assert(Cell.DEAD !== Cell.ALIVE);
    });
  });
});