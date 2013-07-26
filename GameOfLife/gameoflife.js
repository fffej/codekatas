"use strict";

var assert = require('assert');

var DEAD = 0;
var LIVE = 1;

var sampleInput = "4 8\n........\n....*...\n...**...\n........\n";

var createGrid = function(s) {
  var lines = s.split('\n');
  var rc = lines[0].split(' ');
  var rows = rc[0] | 0;
  var cols = rc[1] | 0;

  var game = { rows: rows, cols: cols };

  for (var i=0;i<rows;++i) {
    for (var j=0;j<cols;++j) {
      game[i,j] = DEAD;
    }
  } 

  return game;
};

describe('game of life', function() {
  describe('grid', function() {
    it('should read input', function() {
      var grid = createGrid(sampleInput);

      assert.equal(4, grid.rows);

      assert.equal(DEAD, grid[0,0]);
      assert.equal(LIVE, grid[1,4]);
    });
  });
});
