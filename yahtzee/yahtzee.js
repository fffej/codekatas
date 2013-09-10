"use strict";
var assert=require('assert');

var Yahtzee = function() {
  return this;
};

describe('yahtzee', function() {
  it('exists', function() {
    assert(new Yahtzee());
  });

  it('roll five numbers', function() {
     var yahtzee = new Yahtzee();
     yahtzee.roll(1,2,3,4,5);
     assert.equal(1 + 2 + 3 + 4 + 5, yahtzee.score());
  });
});