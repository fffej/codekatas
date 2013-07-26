"use strict";

var assert = require('assert');

var sampleInput = "4 8\n........\n....*...\n...**...\n........\n";

var createGrid = function(s) {
  var lines = s.split('\n');
  var rc = lines[0].split(' ');
  var rows = rc[0] | 0;
  var cols = rc[1] | 0;

  return { rows: rows, cols: cols };
};

describe('game of life', function() {
  describe('grid', function() {
    it('should read input', function() {
      var grid = createGrid(sampleInput);

      assert.equal(4, grid.rows);
    });
  });
});
