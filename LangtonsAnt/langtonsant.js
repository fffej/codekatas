"use strict";

var assert = require('assert');

var White = 0;
var Black = 1;

var Up = 8;
var Left = 16;
var Down = 24;
var Right = 32;

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

  var orientations = [Up, Left, Down, Right];
  var i = 0;

  return {
    orientation: function() { return orientations[i]; },
    turnLeft: function() { 
      i = (i + 1) % 4;
    },
    turnRight: function() {
      if (i - 1 < 0) {
        i = 3;
      } else {
        i = i - 1;
      }
    }
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

    it('can turn left', function() {
      var ant = new Ant();
      assert.equal(Up, ant.orientation());
  
      ant.turnLeft();
      assert.equal(Left, ant.orientation());

      ant.turnLeft();
      assert.equal(Down, ant.orientation());

      ant.turnLeft();
      assert.equal(Right, ant.orientation());
    });

    it('can turn right', function() {
      var ant = new Ant();
      assert.equal(Up, ant.orientation());
  
      ant.turnRight();
      assert.equal(Right, ant.orientation());

      ant.turnRight();
      assert.equal(Down, ant.orientation());

      ant.turnRight();
      assert.equal(Left, ant.orientation());
    });

  });
  
});  