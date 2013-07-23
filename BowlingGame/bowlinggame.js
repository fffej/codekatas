"use strict";

var assert = require('assert');

var Game = function() {
  return {
    currentFrame: 1,
    isInProgress: true,
    currentScore: 0,
    bowl: function() {
      this.currentFrame++;
    }
  };
};

var testInitialState = function() {
  var game = new Game();
  assert.equal(1, game.currentFrame);
  assert.equal(true, game.isInProgress);
  assert.equal(0, game.currentScore);
};

var testBowlingSingleFrame = function() {
  var game = new Game();
  game.bowl(9,0);
  assert.equal(2, game.currentFrame);
};

var test = function() {
  testInitialState();

  testBowlingSingleFrame();
};

test();