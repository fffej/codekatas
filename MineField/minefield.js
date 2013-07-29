"use strict";

var assert = require('assert');

var Grid = function(h, w) {
  return {
    _mines: [],    

    width: w,
    height: h,

    addMine: function(r,c) {
      this._mines.push(r + ',' + c);
    },

    isMine: function(r,c) {
      return this._mines.indexOf(r + ',' + c) !== -1;
    },

    surroundingMines: function(r,c) {

      var count = 0;

      for (var i=-1;i<=1;++i) {
        for (var j=-1;j<=1;++j) {
          if (i === 0 && j === 0) {
            continue;
          } 
  
          var x = r + i;
          var y = c + j;

          if (this.isMine(x,y)) {
            count++;
          }
        }
      }

      return count;
    }
  };
};

var createGridFromString = function(str) {
  var lines = str.split('\n');
  var hw = lines[0].split(' ');
  var height = hw[0] | 0;
  var width = hw[1] | 0;

  var grid = new Grid(hw[0]|0,hw[1]|0);

  for (var i=0;i<height;++i) {
    var row = lines[i+1];
    for (var j=0;j<width;++j) {
      var isMine = row[j] === '*';
      if (isMine) {
        grid.addMine(j,i);
      }
    }
  }

  return grid;
};

describe('minefield', function() {
  var sampleGrid = '3 4\n*...\n..*.\n....';

  describe('grid', function() {
    it('should be able to create a grid', function() {
      var grid = new Grid(5,6);

      assert.equal(grid.width, 6);
      assert.equal(grid.height,5);
    });

    it('should be able to create a grid of size from a string', function() {
      var grid = createGridFromString(sampleGrid);

      assert.equal(grid.width,4);
      assert.equal(grid.height,3);
    });

    it('reads in the correct values', function() {
      var grid = createGridFromString(sampleGrid);

      assert(grid.isMine(0,0));
      assert(grid.isMine(2,1));
      assert(!grid.isMine(0,1));
    });

    it('knows the surrounding number of mines', function() {
      var grid = createGridFromString(sampleGrid);

      assert.equal(0, grid.surroundingMines(0,2));
      assert.equal(1, grid.surroundingMines(0,1));
      assert.equal(2, grid.surroundingMines(1,0));
    });
  });
});
