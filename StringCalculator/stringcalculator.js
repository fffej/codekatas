"use strict"

var assert = require('assert');

var unsupported = function(x) {
  return x < 0;
};

var supported = function(x) {
  return x < 1000;
};

var coerceToNum = function(x) {
  return x|0;
};

var add = function(str) {
  var delimiter = customDelimiter(str);
  var numbers = this.tokenize(str, delimiter);

  var nums = numbers.map(coerceToNum).filter(supported);
  var illegal = nums.filter(unsupported);
  if (illegal.length > 0) {
    throw new Error('Negative numbers unsupported: ' + illegal);
  }

  return nums.reduce(function(x,y) {
    return x + y;
  }, 0);
};

var tokenize = function(str, delimiter) {
  var s = delimiter.line.replace('\n', delimiter.separator);
  return s.split(delimiter.separator);
};

var StringCalculator = function() {
  this.add = add;
  this.tokenize = tokenize;
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

    it('should not support negative numbers', function() {
      assert.throws(function() {
          calculator.add('-1');
        });
    });

    it('should include all numbers in the message', function() {
      assert.throws(function() {
          calculator.add('-1,-2');
        },
        function(e) {
          var msg = e.message;
          
          assert(msg.indexOf('-1') !== -1,'-1 found');
          assert(msg.indexOf('-2') !== -1,'-2 found');
          return true;
        }
      );
    });

    it('should ignore numbers > 1000', function() {
      assert.equal(2, calculator.add('2,1001'));
    });
  });
});

// What did I learn this time?
// MOAR objects and clearer definition of responsibility has
// benefits even at this scale.
