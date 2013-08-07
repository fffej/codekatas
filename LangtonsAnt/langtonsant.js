"use strict";

var assert = require('assert');

var White = 0;
var Black = 1;

var Up = 8;

var Cell = function() {

  var state = White;

  return {
    value: function() { return state; },
    flip: function() {
      if (state === White) {
        state = Black;
      } else {
        state = White;
      }
      return this;
    }
  };
};

var Grid = function(w,h) {
  return {
    ant: function() { return {}; } 
  };
};

var Ant = function() {
  return {
    orientation: function() { return Up; }
  };
};

describe('langtons ant', function() {
  it('has cells', function() {
    assert(new Cell());
  });

  it('cells start white', function() {
    assert.equal(White, new Cell().value());
  });

  it('cells toggle to black', function() {
    assert.equal(Black, new Cell().flip().value());
  });

  describe('has a grid', function() {
    it('is created from a width and height', function() {
      assert(new Grid(10,10));
    });

    it('has an ant', function() {
      assert(new Grid(10,10).ant());
    });
  });

  describe('ant', function() {
    it('exists', function() {
      assert(new Ant());
    });

    it('has an orientation', function() {
      assert.equal(Up, (new Ant().orientation()));
    });
  });
  
});  