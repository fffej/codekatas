"use strict";

var assert = require('assert');

var Game = function() {
  return {
    score: function() {
      return '0 - 0';
    }
  };
};

describe('tennis', function() {
  describe('game', function() {
    it('starts at 0-0', function() {
      var game = new Game('Joe', 'Fred');
      assert.equal('Joe 0 - Fred 0', game.score());
    });
  });
});
