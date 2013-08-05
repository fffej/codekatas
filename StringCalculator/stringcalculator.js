"use strict";

var assert = require('assert');

var StringCalculator = function() {
  return {
    add: function(str) {
      if (str === '') {
        return 0;
      }

      if (-1 === str.indexOf(",")) {
        return str|0;
      }
    }
  };
};

suite('String calculator', function() {

  suite('creation', function() {
    test('can create', function() {
      assert(new StringCalculator());
    });
  });

  suite('#add', function() {

    var calc = new StringCalculator();

    test('can add', function() {
      assert(calc.add);
    });

    test('empty string returns 0', function() {
      assert.equal(0, calc.add(''));
    });

    test('single numbers return identity', function() {
      assert.equal(0, calc.add('0'));
      assert.equal(101, calc.add('101'));
    });
  });
});

// Learnings
// - use "mocha -u tdd <foo>"