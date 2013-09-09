"use strict";
var assert = require('assert');

var Game = function() {

  this.roll = function(pins) {
    return -1;
  };

  this.score = function() {
    return 0;
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
  });
});