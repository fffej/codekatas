"use strict";

var assert = require('assert');

var Grid = function(w,h) {
  return {
    width: w,
    height: h
  };
};

describe('minefield', function() {
  describe('grid', function() {
    it('should be able to create a grid', function() {
      var grid = new Grid(5,6);

      assert.equal(grid.width, 5);
      assert.equal(grid.height, 6);
    });
  });
});
