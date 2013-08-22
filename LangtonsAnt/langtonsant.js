"use strict";
var assert = require('assert');

var Orientation = {
  NORTH: 0,
  EAST: 1,
  SOUTH: 2,
  WEST: 3
};

var Plane = function() {
  return this;
};

var Ant = function() {

  var orientation = Orientation.NORTH;

  Ant.prototype.position = function() {
    return { x: 0, y: 0 };
  };

  Ant.prototype.orientation = function() {
    return orientation;
  };

  Ant.prototype.turnClockwise = function() {
    var dirs = [
        Orientation.NORTH, 
        Orientation.EAST,
        Orientation.SOUTH,
        Orientation.WEST
    ];

    var i = dirs.indexOf(orientation);

    orientation = dirs[(i+1) % 4];
  };

  return this;
};

describe('infinite grid', function() {
  describe('grid', function() {
    it('exists', function() {
      assert(new Plane());
    });
  });

  describe('ant', function() {
    it('exists', function() {
      assert(new Ant());
    });

    it('has a position', function() {
      assert(new Ant().position());
    });

    it('has an orientation', function() {
      assert.equal(Orientation.NORTH, new Ant().orientation());
    });

    it('can turn clockwise', function() {
      var ant = new Ant();

      ant.turnClockwise();
      assert.equal(Orientation.EAST, ant.orientation());

      ant.turnClockwise();
      assert.equal(Orientation.SOUTH, ant.orientation());

      ant.turnClockwise();
      assert.equal(Orientation.WEST, ant.orientation());

      ant.turnClockwise();
      assert.equal(Orientation.NORTH, ant.orientation());
    });
  });

});
