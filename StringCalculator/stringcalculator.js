"use strict";

var assert = require('assert');

var StringCalculator = function() {
  return {
    add: function() {}
  };
};

suite('String calculator', function() {
  suite('#add', function() {
    test('can create', function() {
      assert(new StringCalculator());
    });

    test('can add', function() {
      var calc = new StringCalculator();
      assert(calc.add);
    });

    test('empty string returns 0', function() {
      assert.equal(0, calc.add(''));
    });
  });
});

// Learnings
// - use "mocha -u tdd <foo>"