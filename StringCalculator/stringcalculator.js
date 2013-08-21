"use strict";

var assert = require('assert');

var StringCalculator = function() {
  StringCalculator.prototype.add = function() { 
    return 0;
  };
  return this;
};

describe('string calculator', function() {
  it('exists', function() {
    assert(new StringCalculator());
  });

  it('has an add method', function() {
    assert(new StringCalculator().add);
  });

  describe('add method', function() {
    var stringCalculator = new StringCalculator();

    it('0 args', function() {
      assert.equal(0, stringCalculator.add(''));
    });

    it('1 arg', function() {
      assert.equal(55, stringCalculator.add('55'));
    });
  });
});