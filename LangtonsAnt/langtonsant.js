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

var Ant = function(x,y) {

  var orientations = [Up, Left, Down, Right];
  var delta = [[0,-1],[-1,0], [0,1],[1,]];
  var px = x || 0;
  var py = y || 0;
  var i = 0;

  return {
    x: function() { return px; },
    y: function() { return py; },
    orientation: function() { return orientations[i]; },
    turnLeft: function() { 
      i = (i + 1) % 4;
      return this;
    },
    turnRight: function() {
      if (i - 1 < 0) {
        i = 3;
      } else {
        i = i - 1;
      }
      return this;
    },
    on: function(cell) {
      if (cell.value() === White) {
        this.turnRight();
        cell.flip();
      } else {
        this.turnLeft();
        cell.flip();
      }
    },
    march: function() {
      var dx = delta[i];
      px += dx[0];
      py += dx[1];
      return this;
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

    it('performs an an action on a white cell', function() {
      var ant = new Ant();

      var called = false;
      var cell = { 
        value: function() { return White; },
        flip: function() { called = true; }
      }
      ant.on(cell);

      assert.equal(true, called);
      assert.equal(Right, ant.orientation());
    });


    it('performs an action on a black cell', function() {
      var ant = new Ant();
      var called = false;
      var cell = { 
        value: function() { return Black; },
        flip: function() { called = true; }
      }
      ant.on(cell);

      assert.equal(true, called);
      assert.equal(Left, ant.orientation());
    });

    it('has a position', function() {
      var ant = new Ant(5,5);
      ant.march();

      assert.equal(5, ant.x());
      assert.equal(4, ant.y());

      ant.turnRight().turnRight().march();
      assert.equal(5, ant.x());
      assert.equal(5, ant.y());
    });
  });
 
});  