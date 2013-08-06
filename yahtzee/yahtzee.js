"use strict";

var assert = require('assert');

var Game = function(dice) {
  if (!dice) {
    dice = [];
    for (var i=0;i<5;++i) {
      dice.push(1 + (Math.random() * 6 | 0));
    }
  }

  return {
    dice: function() {
      return dice;
    },
    score: function(category) {
      return category.score(dice);
    }
  };
};

var Chance = function() {
  return {
    score: function(nums) {
      return nums.reduce(function(x,y) { return x + y; },0);
    }
  };
};
var Yahtzee = function() {};
var Single = function(n) {};
var Pair = function() {};
var TwoPairs = function() {};
var ThreeOfAKind = function() {};
var FourOfAKind = function() {};
var SmallStraight = function() {};
var LargeStraight = function() {};
var FullHouse = function() {};

var categories = [
  new Chance(),
  new Yahtzee(),
  new Single(1),
  new Single(2),
  new Single(3),
  new Single(4),
  new Single(5),
  new Single(6),
  new Pair(),
  new TwoPairs(),
  new ThreeOfAKind(),
  new FourOfAKind(),
  new SmallStraight(),
  new LargeStraight(),
  new FullHouse()
];

describe('yahtzee', function() {
  describe('five dice', function() {
    it('should consist of 5 values between 1 and 6', function() {
      var yahtzee = new Game();
      assert(yahtzee);

      assert.equal(5, yahtzee.dice().length);

      for(var d=0;d<5;d++) {
        assert(yahtzee.dice()[d] > 0 && yahtzee.dice()[d] < 7);
      }
    });
  });

  describe('categories', function() {
    it('should have 15 categories', function() {
      assert.equal(15, categories.length);
    });

    it('always accept chance', function() {
      assert.equal(1 + 2 + 3 + 4 + 5, new Game([1,2,3,4,5]).score(new Chance()));
    });

    it('single works', function() {
      assert.equal(1, new Game[1,2,3,4,5].score(new Single(1)));
      assert.equal(0, new Game[1,2,3,4,5].score(new Single(6)));
    });
  });
});