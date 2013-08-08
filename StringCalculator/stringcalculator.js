"use strict";

var assert = require('assert');

var StringCalculator = function() {

  var replaceNewlinesWithCommas = function(x) {
    return x.replace('\n', ',');
  };

  return {
    add: function(s) {
      s = replaceNewlinesWithCommas(s);
   
      var tokens = s.split(',');
      var sum = 0;
      for (var i=0;i<tokens.length;++i) {
        sum += tokens[i]|0;
      }

      return sum;
    }
  };
};

describe('string calculator', function() {
  it('should exist', function() {
    assert(new StringCalculator());
  });

  it('has an add method', function() {
    assert(new StringCalculator().add);
  });

  describe('add', function() {

    var stringCalculator = new StringCalculator();
    
    it('returns the empty string for 0', function() {
      assert.equal(0, stringCalculator.add(''));
    });

    it('is identity for a single number', function() {
      assert.equal(123, stringCalculator.add('123'));
    });

    it('adds two numbers', function() {
      assert.equal(4, stringCalculator.add('1,3'));
    });

    it('adds all numbers', function() {
      assert.equal(6, stringCalculator.add('1,2,3'));
    });

    it('treats newlines as delimiters', function() {
      assert.equal(4, stringCalculator.add('1\n3'));
    });

    it('supports custom delimiters', function() {
      assert.equal(5, stringCalculator.add('//q\n1q4'));
    });
  });
});