"use strict";

var assert = require('assert');

var Grid = function(h, w) {
  return {
    width: w,
    height: h,

    addMine: function(r,c) {

    },

    isMine: function(r,c) {
      return true;
    }
  };
};

var createGridFromString = function(str) {
  var lines = str.split('\n');
  var hw = lines[0].split(' ');
  var height = hw[0] | 0;
  var width = hw[1] | 0;

  for (var i=0;i<height;++i) {
    var row = lines[i+1];
    for (var j=0;j<width;++j) {
      var isMine = row[j] === '*';
    }
  }

  var grid = new Grid(hw[0]|0,hw[1]|0);

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
    });
  });
});
