"use strict";

var assert = require('assert');

var DEAD = 0;
var LIVE = 1;

var sampleInput = "4 8\n........\n....*...\n...**...\n........\n";

var createGrid = function(s) {
  var lines = s.split('\n');
  var rc = lines[0].split(' ');
  var rows = rc[0] | 0; // 4
  var cols = rc[1] | 0; // 8

  var game = { rows: rows, cols: cols };

  for (var i=0;i<rows;++i) {
    for (var j=0;j<cols;++j) {
      var v = lines[1+i][j];
      if (game[j] === undefined) { game[j] = []; }
      game[j][i] = v === '*' ? LIVE : DEAD;
    }
  } 

  return game;
};

describe('game of life', function() {
  describe('cell', function() {
    it('dies with underpopulation', function() {
      assert.equal(DEAD, cellState(LIVE,2));
    });
  });

  describe('grid', function() {
    it('should read input', function() {
      var grid = createGrid(sampleInput);

      assert.equal(4, grid.rows);

      assert.equal(DEAD, grid[0][0]);
      assert.equal(LIVE, grid[4][1]);
      assert.equal(LIVE, grid[3][2]);
      assert.equal(LIVE, grid[4][2]);
    });
  });
});

