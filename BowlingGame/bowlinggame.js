"use strict";
var assert = require('assert');

var Game = function() {

  this.roll = function(pins) {
    return -1;
  };

  return this;
};

describe('bowling game', function() {
  it('is able to create a game', function() {
    assert(new Game());
  });

  it('is able to bowl a zero', function() {
    assert(new Game().roll(0));
  });
});