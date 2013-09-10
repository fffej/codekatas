"use strict";
var assert=require('assert');

var Category = {
  Chance: 1
};

var areEqual = function(a,b) {
  if (a.length !== b.length) {
    return false;
  }

  for (var i=0;i<a.length;++i) {
    if (a[i] !== b[i]) {
      return false;
    }
  }

  return true;
};

var Yahtzee = function() {

  var dice = [];

  this.roll = function(a,b,c,d,e) {
    dice = [a,b,c,d,e].sort();
  };

  this.score = function(category) {
    switch (category) {
      case Category.Chance:
        var sum = 0;
        for (var i=0;i<dice.length;++i) {
          sum += dice[i];
        }
        return sum;
      case Category.SmallStraight:
        if (areEqual(dice,[1,2,3,4,5])) {
          return 15;
        } else {
          return 0;
        }
    }
  };

  return this;
};

describe('yahtzee', function() {
  it('exists', function() {
    assert(new Yahtzee());
  });

  it('roll five numbers as chance', function() {
     var yahtzee = new Yahtzee();
     yahtzee.roll(1,2,3,4,5);
     assert.equal(1 + 2 + 3 + 4 + 5, yahtzee.score(Category.Chance));
  });

  it('roll five numbers as low-straight', function() {
    var yahtzee = new Yahtzee();
    yahtzee.roll(1,2,3,4,5);

     assert.equal(15, yahtzee.score(Category.SmallStraight));
  });

  it('roll five numbers as low-straight', function() {
    var yahtzee = new Yahtzee();
    yahtzee.roll(1,3,3,4,4);

     assert.equal(0, yahtzee.score(Category.SmallStraight));
  });


});