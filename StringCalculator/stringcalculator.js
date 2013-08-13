"use strict"

var assert = require('assert');

var StringCalculator = function() {

};

describe('string calculator', function() {
  describe('simple add', function() {

    var calculator = new StringCalculator();

    it('should return 0 for empty string', function() {
      assert.equal(0, calculator.add(''));     
    });
  });
});
