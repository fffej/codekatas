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
      return category(dice);
    }
  };
};

var strictAdd = function(x,y) { return x + y; };
var isN = function(x) { return function(z) { return x === z; }; };

var chance = function(nums) {
  return nums.reduce(strictAdd,0);
};

var single = function(n) {
  return function(nums) {
    return nums.filter(isN(n)).reduce(strictAdd,0);
  };
};

var twoPairs = function(nums) {
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
};  


var nOfAKind = function(n, nums) {
  for (var i=6;i>=1;i--) {
    if (nums.filter(isN(i)).length >= n) {
       return n * i;
    }
  }
  return 0;
};

var pair = function(nums) { return nOfAKind(2,nums); };

var threeOfAKind = function(nums) { return nOfAKind(3,nums); };

var fourOfAKind = function(nums) { return nOfAKind(4,nums); };

var yahtzee = function(nums) { return nOfAKind(5,nums) ? 50 : 0; };

var matchesGoal = function(nums,goal) {
  var s = nums.sort();
  for (var i=0;i<5;++i) {
    if (s[i] !== goal[i]) {
      return false;
    }
  }
  return true;
};

var smallStraight = function(nums) {
  return matchesGoal(nums,[1,2,3,4,5]) ? 15 : 0;
};

var largeStraight = function(nums) {
  return matchesGoal(nums,[2,3,4,5,6]) ? 20 : 0;
};

var FullHouse = function() {};

var categories = [
  chance,
  yahtzee,
  single(1),
  single(2),
  single(3),
  single(4),
  single(5),
  single(6),
  pair,
  twoPairs,
  threeOfAKind,
  fourOfAKind,
  smallStraight,
  largeStraight,
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
      assert.equal(1 + 2 + 3 + 4 + 5, new Game([1,2,3,4,5]).score(chance));
    });

    it('single works', function() {
      assert.equal(1, new Game([1,2,3,4,5]).score(single(1)));
      assert.equal(0, new Game([1,2,3,4,5]).score(single(6)));
    });

    it('pair', function() {
      assert.equal(12, new Game([6,6,1,2,3]).score(pair));
      assert.equal(0,  new Game([1,2,3,4,5]).score(pair));
    });

    it('two pairs', function() {
      assert.equal(12 + 10, new Game([6,6,5,5,1]).score(twoPairs));
      assert.equal(0, new Game([6,6,5,3,1]).score(twoPairs));
    });

    it('three of a kind', function() {
      assert.equal(9, new Game([3,3,3,3,1,2]).score(threeOfAKind));
      assert.equal(0, new Game([1,2,3,4,5]).score(threeOfAKind));
    });

    it('four of a kind', function() {
      assert.equal(12, new Game([3,3,3,3,0]).score(fourOfAKind));
      assert.equal(0, new Game([3,3,1,3,0]).score(fourOfAKind));
    });

    it('yahtzee', function() {
      assert.equal(50, new Game([1,1,1,1,1]).score(yahtzee));
      assert.equal(0 , new Game([1,2,3,4,5]).score(yahtzee));
    });

    it('small straight', function() {
      assert.equal(15, new Game([1,2,3,4,5]).score(smallStraight));
      assert.equal(0, new Game([1,2,3,4,6]).score(smallStraight));
    });

    it('large straight', function() {
      assert.equal(20, new Game([2,3,4,5,6]).score(largeStraight));
      assert.equal(0, new Game([1,2,3,4,6]).score(largeStraight));
    });

    it('full house', function() {
      assert.equal(3*6 + 2*5, new Game([5,6,6,6,5]).score(fullHouse));
    });
  });
});