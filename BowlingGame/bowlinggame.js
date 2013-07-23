"use strict";

var assert = require('assert');

var Game = function() {
  return {
    currentFrame: 1,
    isInProgress: true,
    currentScore: 0
  };
};

var testInitialState = function() {
  var game = new Game();
  assert.equal(1, game.currentFrame);
  assert.equal(true, game.isInProgress);
  assert.equal(0, game.currentScore);
};

var test = function() {
  testInitialState();
};

test();