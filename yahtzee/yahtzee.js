"use strict";

var assert = require('assert');

describe('yahtzee', function() {
  describe('five dice', function() {
    it('should consist of 5 values between 1 and 6', function() {
      var yahtzee = new Yahtzee();
      assert(yahtzee);
    });
  });
});