"use strict";

var assert = require('assert');

var White = 0;
var Black = 1;

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
}

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
});