"use strict";

var assert = require('assert');

var bowl = function(hits) {
  
};

var test = function() {
  var game = new Game();
  assert.equal(1, game.currentFrame);
  assert.equal(true, game.isInProgress);
  assert.equal(0, game.currentScore);
};

test();