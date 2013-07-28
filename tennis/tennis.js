"use strict";

var assert = require('assert');

var formatScore = function(player, score) {
  return player + ' ' + score;
};

var Game = function(player1,player2) {
  return {
    _serverPoints: 0,
    _otherPoints: 0,

    score: function() {
      return formatScore(player1, this._serverPoints) +
             ' - ' + 
             formatScore(player2, this._otherPoints);
    },

    serverPoint: function() {
      var pointsProgression = [0,15,30,40];
      var i = pointsProgression.indexOf(this._serverPoints);
      this._serverPoints = pointsProgression[i+1];
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

      game.serverPoint();
      assert.equal('Joe 30 - Fred 0', game.score());      

      game.serverPoint();
      assert.equal('Joe 40 - Fred 0', game.score());
    });
  });
});
