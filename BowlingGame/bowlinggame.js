"use strict";
var assert = require('assert');

var Game = function() {

  var rolls = [];

  this.roll = function(pins) {
    rolls.push(pins);
  };

  this.score = function() {
    var score = 0;
    for (var i=0;i<10;++i) {
      var frameScore = rolls[2*i] + rolls[2*i+1];
      if (frameScore === 10) { 
        score += frameScore + rolls[2*i+2];
      }
      else {
        score += frameScore;
      }
    }

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

    var rollMany = function(n, pins) {
      for (var i=0;i<n;++i) {
        game.roll(pins);
      }
    }

    it('is able to bowl a gutter game', function() {
      rollMany(20,0);

      assert.equal(0, game.score());
    });

    it('scores non strike pins', function() {
      game.roll(5);
      rollMany(19,0);

      assert.equal(5, game.score());
    });

    it('successfully scores a spare', function() {
      game.roll(3);
      game.roll(7);
      game.roll(2);

      rollMany(17,0);
      assert.equal(3 + 7 + 2 + 2, game.score());
    });
  });
});