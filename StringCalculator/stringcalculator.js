"use strict";

var assert = require('assert');

var sumArrayOfStringsAsInts = function(n) {
    var sum = 0;
    for (var i=0;i<n.length;++i) {
      sum += n[i]|0;
    }
    return sum;
};

var tokenize = function(s) {
  s = s.replace(/\n/g, ',');
  return s.split(',');
};

var StringCalculator = function() {
  StringCalculator.prototype.add = function(s) { 
    if (s.length === 0) {
      return 0;
    }

    var tokens = tokenize(s);
    return sumArrayOfStringsAsInts(tokens);
  };
  return this;
};

describe('string calculator', function() {
  it('exists', function() {
    assert(new StringCalculator());
  });

  it('has an add method', function() {
    assert(new StringCalculator().add);
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

    it('supports different delimiters', function() {
      assert.equal(1+2+3, stringCalculator.add('//q\n1q2q3'));
    });
  });
});