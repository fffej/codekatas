"use strict";

var assert = require('assert');


var Game = function(player1,player2) {

  var WIN = 101; // sentinel

  var formatPlayerScore = function(player, score) {
    return player + ' ' + score;
  };

  return {
    _withServes: 0,
    _otherPoints: 0,

    score: function() {
      if (this._withServes === WIN) {
        return player1 + ' won';
      }
      
      return formatPlayerScore(player1, this._withServes) +
             ' - ' + 
             formatPlayerScore(player2, this._otherPoints);
    },

    withServe: function() {
      var pointsProgression = [0,15,30,40,WIN];
      var i = pointsProgression.indexOf(this._withServes);
      this._withServes = pointsProgression[i+1];
    },

    againstServe: function() {
      this._otherPoints += 15;
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

      game.withServe();
      assert.equal('Joe 15 - Fred 0', game.score());

      game.withServe();
      assert.equal('Joe 30 - Fred 0', game.score());      

      game.withServe();
      assert.equal('Joe 40 - Fred 0', game.score());

      game.withServe();
      assert.equal('Joe won', game.score());
    });

    it('game against server to love', function() {
      var game = new Game('Joe', 'Fred');

      game.againstServe();
      assert.equal('Joe 0 - Fred 15', game.score());      
    });
  });
});
