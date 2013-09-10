"use strict";
var assert=require('assert');

var Category = {
  Chance: 1
};

var Yahtzee = function() {

  var dice = [];

  this.roll = function(a,b,c,d,e) {
    dice = [a,b,c,d,e];
  };

  this.score = function(category) {
    var sum = 0;
    for (var i=0;i<dice.length;++i) {
      sum += dice[i];
    }
    return sum;
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