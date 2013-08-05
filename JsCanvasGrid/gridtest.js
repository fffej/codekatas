"use strict";

var assert = require('assert');
var g = require('./grid.js');

describe('grid view', function() {
  it('is created from a grid', function() {
    var gridView = new g.GridView(new g.Grid(5,6));
    assert(gridView);
  });
});

describe('grid', function() {
  it('has a width and height', function() {
    var grid = new g.Grid(5,6);

    assert.equal(5, grid.width());
    assert.equal(6, grid.height());
  });

  it('each cell is initally \'off\'', function() {
    var grid = new g.Grid(5,6);
    assert.equal(g.Off, grid.state(3,3));
  });

  it('each cell can be toggled', function() {
    var grid = new g.Grid(5,6);

    assert.equal(g.Off, grid.state(3,3));
    grid.toggle(3,3);
    assert.equal(g.On, grid.state(3,3));
    grid.toggle(3,3);
    assert.equal(g.Off, grid.state(3,3));
  });
});
