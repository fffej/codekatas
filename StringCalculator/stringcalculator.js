"use strict";

var assert = require('assert');

var StringCalculator = function() {

  var replaceNewlinesWithCommas = function(x) {
    return x.replace('\n', ',');
  };

  var tokenize = function(x,delim) {
    return x.split(delim);
  };

  var isCustomDelimiter = function(x) {
     var delimEnd = x.indexOf('\n');
     if (x.indexOf('//') === 0) {
       return x.substr(2,delimEnd-2);
     } else {
       return ',';
     }
  };

  var stripDelimiter = function(x) {
    var delimEnd = x.indexOf('\n');
    if (x.indexOf('//') === 0) { 
      return x.substr(delimEnd+1);
    } else {
      return x;
    }
  };

  var tidyString = function(x) {
    return replaceNewlinesWithCommas(stripDelimiter(x));
  };

  var shouldIgnore = function(n) { return n > 1000; };
  var isIllegal = function(n) { return n < 0; };
  
  return {
    add: function(s) {

      var delimiter = isCustomDelimiter(s);
      s = tidyString(s);
      var tokens = tokenize(s, delimiter);
      var sum = 0;

      var illegal = [];

      for (var i=0;i<tokens.length;++i) {
        var num = tokens[i]|0;
        if (shouldIgnore(num)) {
          continue;
        }
     
        if (isIllegal(num)) {
          illegal.push(num);
        }

        sum += num;
      }

      if (illegal.length !== 0) {
        throw new Error(illegal);
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

    it('disallows negatives', function() {
      assert.throws(function() { stringCalculator.add('-1'); });
    });

    it('reports all', function() {
      assert.throws(function() { stringCalculator.add('-1\n-2'); }, function(x) { 
        return x.message.indexOf('-1,-2') >= 0;
      });
    });

    it('numbers greater than 1000 ignored', function() {
      assert.equal(5, stringCalculator.add('5,1005'));
    });

    it('supports multiple delimiters', function() {
      assert.equal(4, stringCalculator.add('//**\n1**3'));
    });
  });
});