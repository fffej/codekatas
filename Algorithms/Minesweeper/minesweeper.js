"use strict";
var assert = require('assert');

var Minesweeper = function(s) {

  return this;
};

describe('minesweeper', function() {

  var grid1 = '4 4\n*...\n....\n....\n.*..\n';
  var grid2 = '3 5\n**...\n.....\n.*...\n';

  describe('can be created from string', function() {
    it('can create from string', function() {
      var grid = new Minesweeper(grid1);
      assert(grid);
    });
  });
});
