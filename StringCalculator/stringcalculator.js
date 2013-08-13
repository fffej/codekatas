"use strict"

var assert = require('assert');

var add = function(str) {
  return str|0;
};

var StringCalculator = function() {
  this.add = add;
  return this;
};

describe('string calculator', function() {
  describe('simple add', function() {

    var calculator = new StringCalculator();

    it('should return 0 for empty string', function() {
      assert.equal(0, calculator.add(''));     
    });

    it('should return a single digit', function() {
      assert.equal(99, calculator.add('99'));
    });
  });
});
