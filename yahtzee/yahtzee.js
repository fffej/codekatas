"use strict";

var assert = require('assert');

var Yahtzee = function() {

  var dice = [];
  for (var i=0;i<5;++i) {
    dice.push(1 + (Math.random() * 6 | 0));
  }

  return {
    dice: function() {
      return dice;
    }
  };
};

describe('yahtzee', function() {
  describe('five dice', function() {
    it('should consist of 5 values between 1 and 6', function() {
      var yahtzee = new Yahtzee();
      assert(yahtzee);

      assert.equal(5, yahtzee.dice().length);
    });
  });
});