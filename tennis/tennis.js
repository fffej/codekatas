"use strict";

var assert = require('assert');


var Game = function(player1,player2) {

  var WIN = 101; // sentinel

  var pointsProgression = [0,15,30,40,WIN];

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
      else if (this._otherPoints === WIN) {
        return player2 + ' won';
      }
      
      return formatPlayerScore(player1, this._withServes) +
             ' - ' + 
             formatPlayerScore(player2, this._otherPoints);
    },

    _returnToDeuce: function() {
      this._withServes = this._otherPoints = 40;
    },

    _advancePoint: function(prop,other) {

      // Return to deuce
      if (this[other] === 'A' && this[prop] === 40) {
        this._returnToDeuce();
        return;
      }
     
      // Win from advantage
      if (this[prop] === 'A') {
        this[prop] = WIN;
        return;
      }

      // Advantage from deuce
      if (this[other] === 40 && this[prop] === 40) {
        this[prop] = 'A';
        return;
      }

      // Move along
      var i = pointsProgression.indexOf(this[prop]);
      this[prop] = pointsProgression[i+1];
    },

    withServe: function() {
      this._advancePoint('_withServes', '_otherPoints');
    },

    againstServe: function() {
      this._advancePoint('_otherPoints', '_withServes');
    }
  };
};

var gameAtDeuce = function() {
  var game = new Game('Joe', 'Fred');
  game.withServe(); game.againstServe();
  game.withServe(); game.againstServe();
  game.withServe(); game.againstServe();

  return game;
};

describe('tennis', function() {
  describe('game', function() {
    it('starts at 0-0', function() {
      var game = new Game('Joe', 'Fred');
      assert.equal('Joe 0 - Fred 0', game.score());
    });

    it('handles deuce', function() {
      var game = gameAtDeuce();
      assert.equal('Joe 40 - Fred 40', game.score());

      game.withServe();
      assert.equal('Joe A - Fred 40', game.score());

      game.againstServe();
      assert.equal('Joe 40 - Fred 40', game.score());

      game.againstServe();
      assert.equal('Joe 40 - Fred A', game.score());

      game.againstServe();
      assert.equal('Fred won', game.score());
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

      game.againstServe();
      assert.equal('Joe 0 - Fred 30', game.score());

      game.againstServe();
      assert.equal('Joe 0 - Fred 40', game.score());

      game.againstServe();
      assert.equal('Fred won', game.score());
    });
  });
});

