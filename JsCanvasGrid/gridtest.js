"use strict";

var assert = require('assert');
var g = require('./grid.js');

describe('grid', function() {
  it('has a width and height', function() {
    var grid = new g.Grid(5,6);

    assert.equal(5, grid.width());
    assert.equal(6, grid.height());
  });
});
