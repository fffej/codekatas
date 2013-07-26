"use strict";

var assert = require('assert');

var sampleInput = "4 8\n........\n....*...\n...**...\n........\n";


describe('game of life', function() {
  describe('grid', function() {
    it('should read input', function() {
      var grid = createGrid(sampleInput);

      assert.equal(4, grid.rows);
    });
  });
});
