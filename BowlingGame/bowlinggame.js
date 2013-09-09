"use strict";
var assert = require('assert');

var Game = function() {

  var score = 0;

  this.roll = function(pins) {
    score += pins;
  };

  this.score = function() {
    return score;
  };

  return this;
};

describe('bowling game', function() {
  describe('game', function() {

    var game;

    beforeEach(function() {
      game = new Game();
    });

    it('is able to bowl a gutter game', function() {
      for (var i=0;i<20;++i) {
        game.roll(0);
      }

      assert.equal(0, game.score());
    });

    it('scores non strike pins', function() {
      game.roll(5);
      for (var i=0;i<19;++i) {
        game.roll(0);
      }

      assert.equal(5, game.score());
    });
  });
});