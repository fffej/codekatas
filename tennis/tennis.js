"use strict";

var assert = require('assert');

var Game = function(player1,player2) {
  return {
    score: function() {
      return player1 + ' 0 - ' + player2 + ' 0';
    }
  };
};

describe('tennis', function() {
  describe('game', function() {
    it('starts at 0-0', function() {
      var game = new Game('Joe', 'Fred');
      assert.equal('Joe 0 - Fred 0', game.score());
    });

    it ('game to love', function() {
      var game = new Game('Joe', 'Fred');
      game.serverPoint();
  
      assert.equal('Joe 15 - Fred 0', game.score());
    });
  });
});
