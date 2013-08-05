"use strict";

var assert = require('assert');

var StringCalculator = function() {
  return {
    add: function(str) {
      if (str === '') {
        return 0;
      }

      // extract out delimiter
      var nums;
      if (str.indexOf('//') === 0) {
        var lines = str.split('\n');
        var delim = lines[0].substr(2);
        lines.shift();
        str = lines.join();

        nums = str.split(delim);
      }
      else {
        str = str.replace(/(\n)+/g, ',');
        nums = str.split(',');
      }
      
      var sum = 0;
      for (var i=0;i<nums.length;++i) {
        sum += (nums[i]|0);
      }

      return sum;
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

    test('two numbers', function() {
      assert.equal(2, calc.add('1,1'));
      assert.equal(1001, calc.add('999,2'));
    });

    test('multiple numbers', function() {
      assert.equal(6, calc.add('1,2,3'));
    });

    test('allow new lines', function() {
      assert.equal(2, calc.add('1\n1'));
    });

    test('allow mixture', function() {
      assert.equal(6, calc.add('1,2\n3'));
    });

    test('different delimiters', function() {
      assert.equal(6, calc.add("//q\n1q2q3"));
    });
  });
});

// Learnings
// - use "mocha -u tdd <foo>"