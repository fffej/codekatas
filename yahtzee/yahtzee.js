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

var strictAdd = function(x,y) { return x + y; };
var isN = function(x) { return function(z) { return x === z; }; };

var Chance = function() {
  return {
    score: function(nums) {
      return nums.reduce(strictAdd,0);
    }
  };
};
var Yahtzee = function() {};

var Single = function(n) {
  return {
    score: function(nums) {
      return nums.filter(isN(n)).reduce(strictAdd,0);
    }
  }
};

var Pair = function() {
  return { 
    score: function(nums) {
      for (var i=6;i>=1;i--) {
        if (nums.filter(isN(i)).length >= 2) {
          return 2 * i;
        }
      }
      return 0;
    }
  };
};
var TwoPairs = function() {
  return {
    score: function(nums) {
      var pairs = [];

      for (var i=6;i>=1;i--) {
        var matchingI = nums.filter(isN(i)).length;

        if (matchingI === 4) {
          return 4 * i;
        } else if (matchingI >= 2) {
          pairs.push(2*i);
        }
      }

      if (pairs.length === 2) {
        return pairs[0] + pairs[1];
      } else { 
        return 0;
      }
    }
  };
};
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
      assert.equal(1, new Game([1,2,3,4,5]).score(new Single(1)));
      assert.equal(0, new Game([1,2,3,4,5]).score(new Single(6)));
    });

    it('pair', function() {
      assert.equal(12, new Game([6,6,1,2,3]).score(new Pair()));
      assert.equal(0,  new Game([1,2,3,4,5]).score(new Pair()));
    });

    it('two pairs', function() {
      assert.equal(12 + 10, new Game([6,6,5,5,1]).score(new TwoPairs()));
      assert.equal(0, new Game([6,6,5,3,1]).score(new TwoPairs()));
    });
  });
});