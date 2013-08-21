"use strict";

var assert = require('assert');

var sumArrayOfStringsAsInts = function(n) {
    var sum = 0;
    for (var i=0;i<n.length;++i) {
      if ((n[i]|0) < 0) {
        throw Error('Negatives not allowed');
      }
      sum += n[i]|0;
    }
    return sum;
};

var tokenize = function(s, delim) {
  s = s.replace(/\n/g, delim);
  return s.split(delim);
};

var StringCalculator = function() {
  StringCalculator.prototype.add = function(s) { 
    if (s.length === 0) {
      return 0;
    }

    var result = getDelimiter(s);
    var tokens = tokenize(result.line,result.delimiter);
    return sumArrayOfStringsAsInts(tokens);
  };
  return this;
};

var hasCustomDelimiter = function(s) {
  return s.indexOf('//') === 0;
};

var getDelimiter = function(s) {
  if (hasCustomDelimiter(s)) {
    return { 
      delimiter: s.substr(2, s.indexOf('\n') - 2),
      line: s.substr(s.indexOf('\n') + 1)
    };
  } 

  return { delimiter: ',', line: s };
};

describe('string calculator', function() {
  it('exists', function() {
    assert(new StringCalculator());
  });

  it('has an add method', function() {
    assert(new StringCalculator().add);
  });

  describe('delimiters', function() {
    it('knows about custom delimters', function() {
      assert(hasCustomDelimiter('//q\n1q2q3'));
    });

    it('ignores normal delimters', function() {
      assert(!hasCustomDelimiter('1,2,3'));
    });

    it('returns delimiter', function() {
      var s = getDelimiter('//q\n1q2q3');
      assert('q', s.delimiter);
    });

    it('returns default delimiters', function() {
      var s = getDelimiter('1,2,3');
      assert(',', s.delimiter);
    });
 
    it('consumes some of the string', function() {
      assert.equal('1,2,3', getDelimiter('1,2,3').line);
      assert.equal('1q2q3', getDelimiter('//q\n1q2q3').line);
    });
  });

  describe('add method', function() {
    var stringCalculator = new StringCalculator();

    it('0 args', function() {
      assert.equal(0, stringCalculator.add(''));
    });

    it('1 arg', function() {
      assert.equal(55, stringCalculator.add('55'));
    });

    it('2 args', function() {
      assert.equal(74, stringCalculator.add('69,5'));
    });

    it('n args', function() {
      assert.equal(1+2+3+4+5, stringCalculator.add('1,2,3,4,5'));
    });

    it('handles new lines as separators', function() {
      assert.equal(1+2+3, stringCalculator.add('1\n2\n3'));
    });

    it('supports delimiters', function() {
      assert.equal(1+2+3, stringCalculator.add('//q\n1q2q3'));
    });

    it('dislikes negatives', function() {
      assert.throws(function() {
        stringCalculator.add('1,-2,3');
      });
    });

    it('lists all negatives', function() {
      assert.throws(function() { stringCalculator.add('-1,-2,-3'); }, function(e) { return e.message.indexOf('-1,-2,-3') !== -1; });
    });
  });
});