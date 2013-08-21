"use strict";

var assert = require('assert');

var StringCalculator = function() {
  return this;
};

describe('string calculator', function() {
  it('exists', function() {
    assert(new StringCalculator());
  });
});