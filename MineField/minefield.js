"use strict";

var assert = require('assert');

var Grid = function(w,h) {
  return {
    width: w,
    height: h
  };
};

describe('minefield', function() {
  var sampleGrid = '3 4\n*...\n..*.\n....';

  describe('grid', function() {
    it('should be able to create a grid', function() {
      var grid = new Grid(5,6);

      assert.equal(grid.width, 5);
      assert.equal(grid.height,6);
    });

    it('should be able to create a grid from a string', function() {
      var grid = createGridFromString(sampleGrid);

      assert.equal(grid.width,4);
      assert.equal(grid.height,3);
    });
  });
});