"use strict";

var assert = require('assert');

var StringCalculator = function() {
  return {};
};

describe('string calculator', function() {
  it('should exist', function() {
    assert(new StringCalculator());
  });

  it('has an add method', function() {
    assert(new StringCalculator().add);
  });
});