"use strict";
var assert = require('assert');

var Game = function() {

  this.roll = function(pins) {
    return -1;
  };

  return this;
};

describe('bowling game', function() {
  describe('game', function() {

    var game;

    beforeEach(function() {
      game = new Game();
    });

    it('is able to bowl a zero', function() {
      assert(game.roll(0));
    });
  });
});