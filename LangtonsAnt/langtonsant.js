"use strict";
var assert = require('assert');

var Plane = function() {
  return this;
};

var Ant = function() {
  Ant.prototype.position = function() {
    return { x: 0, y: 0 };
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
  });

});
