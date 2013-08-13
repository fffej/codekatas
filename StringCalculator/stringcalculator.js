"use strict"

var assert = require('assert');

var add = function(str) {
  var delimiter = customDelimiter(str);

  str = delimiter.line.replace('\n',delimiter.separator);
  var numbers = str.split(delimiter.separator);
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
    return {
      line: str.substr(str.indexOf('\n')+1),
      separator: str[2]
    };
  } else {
    return {
      line: str,
      separator: ','
    };
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
      var delim = customDelimiter('//q\n1q2');
      assert.equal('q', delim.separator);
      assert.equal('1q2', delim.line);
    });

    it('should return default separator', function() {
      var delim = customDelimiter('1,2');
      assert.equal(',', delim.separator);
      assert.equal('1,2', delim.line);
    });

    it('should add using the custom delimiter', function() {
      assert.equal(3, calculator.add('//q\n1q2'));
    });
  });
});
