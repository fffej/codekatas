"use strict";
var assert = require('assert');

var Game = function() {
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