"use strict";

var assert = require('assert');

var StringCalculator = function() {
  return {};
};

suite('String calculator', function() {
  suite('#add', function() {
    test('can create', function() {
      assert(new StringCalculator());
    });
  });
});

// Learnings
// - use "mocha -u tdd <foo>"