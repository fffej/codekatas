"use strict";

var assert = require('assert');

var StringCalculator = function() {
  StringCalculator.prototype.add = function() { };
  return this;
};

describe('string calculator', function() {
  it('exists', function() {
    assert(new StringCalculator());
  });

  it('has an add method', function() {
    assert(new StringCalculator().add);
  });
});