"use strict";
var assert = require('assert');

var Minesweeper = function(s) {
  var lines = s.split('\n');
  var xy = lines[0].split(' ');
  var x = xy[0] | 0;
  var y = xy[1] | 0;

  this.height = x;
  this.width = y;

  var grid = new Array(x);
  for (var i=0;i<x;++i) {
    grid[i] = new Array(y);
    for (var j=0;j<y;++j) {
      grid[i][j] = lines[i+1][j] === '*';
    }
  }

  this.at = function(i,j) {
    return grid[j][i];
  };

  return this;
};

describe('minesweeper', function() {

  var grid1 = '4 4\n*...\n....\n..*.\n....\n';
  var grid2 = '3 5\n**...\n.....\n.*...\n';

  describe('can be created from string', function() {
    it('can create from string', function() {
      var grid = new Minesweeper(grid1);
      assert(grid);
    });

    it('width and height are read correctly', function() {
      var grid = new Minesweeper(grid2);
      assert.equal(3,grid.height);
      assert.equal(5,grid.width);
    });

    it('has a grid', function() {
      var grid = new Minesweeper(grid2);
      assert(grid.at(0,0));
      assert(grid.at(1,0));
      assert(!grid.at(2,0));
    });
  });
});
