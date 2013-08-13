"use strict"

var assert = require('assert');

var add = function(str) {
  str = str.replace('\n',',');
  var numbers = str.split(',');
  return numbers.reduce(function(x,y) {
    return x + (y|0);
  }, 0);
};

var StringCalculator = function() {
  this.add = add;
  return this;
};

var customDelimiter = function(str) {
  if (str.indexOf('//') === 0) {
    return str[2];
  }
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

    it('should add numbers separaetd by commas', function() {
      assert.equal(101, calculator.add('77,24'));
    });

    it('should add a variable number of commas', function() {
      assert.equal(10, calculator.add('1,2,3,4'));
    });

    it('should also consider \\n a separate', function() {
      assert.equal(10, calculator.add('6\n4'));
    });

    it('should identify custom delimiters', function() {
      assert.equal('q', customDelimiter('//q\n1q2'));
      assert(!customDelimiter('1\n2'));
    })
  });
});
