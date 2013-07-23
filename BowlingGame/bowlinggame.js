"use strict";

var assert = require('assert');

var Game = function() {
  return {
    currentFrame: 1,
    isInProgress: true,
    currentScore: 0,
    bowl: function(ball1,ball2) {
      ball1 = ball1|0;
      ball2 = ball2|0;

      if (this.wasStrike) {
        this.currentScore += (10 + ball1 + ball2 + ball1 + ball2);
      } 
      else if (this.wasSpare) {
        this.currentScore += (10 + ball1 + ball1 + ball2);
      }
      else if (ball1 === 10) {
        this.wasStrike = true;
      } else if (ball1 + ball2 === 10) {
        this.wasHalf = true;
      } else {
        this.currentScore += (ball1 + ball2);
        this.wasHalf = this.wasStrike = false;
      }

      this.currentFrame++;
    },
    wasHalf: false,
    wasSpare: false
  };
};

var testInitialState = function() {
  var game = new Game();
  assert.equal(1, game.currentFrame);
  assert.equal(true, game.isInProgress);
  assert.equal(0, game.currentScore);
};

var testBowlingSingleFrameCompletesFrame = function() {
  var game = new Game();
  game.bowl(9,0);
  assert.equal(2, game.currentFrame);
};

var testScoringNonStrikeFrame = function() {
  var game = new Game();
  game.bowl(9,0);
  assert.equal(false, game.wasStrike);
  assert.equal(9, game.currentScore);
};

var testStrike = function() {
  var game = new Game();
  game.bowl(10);
  assert.equal(0, game.currentScore);
  assert.equal(true, game.wasStrike);
};

var testWasHalfStrike = function() {
  var game = new Game();
  game.bowl(2,8);
  assert.equal(true, game.wasHalf);
};

var testStrikeScoring = function() {
  var game = new Game();
  game.bowl(10);
  assert.equal(true, game.wasStrike);
  assert.equal(0, game.currentScore);

  game.bowl(2,3);
  assert.equal(10 + 2 + 3 + 2 + 3, game.currentScore);
};

var testStrikeAdvancesFrame = function() {
  var game = new Game();
  game.bowl(10);
  assert.equal(2, game.currentFrame); 
};

var testScoreHalf = function() {
  var game = new Game();
  game.bowl(2,8);
  game.bowl(1,9);

  assert.equal(21, game.currentScore);
};

var test = function() {
  testInitialState();

  testBowlingSingleFrameCompletesFrame();

  testScoringNonStrikeFrame();

  testStrike();
  
  testStrikeAdvancesFrame();

  testStrikeScoring();

  testWasHalfStrike();
};

test();