"use strict";

var assert = require('assert');

var zero = ' _ \n' +
           '| |\n' +
           '|_|\n';

var one = '   \n' +
          '  |\n' + 
          '  |\n';

var two = ' _ \n'+
          ' _|\n'+
          '|_ \n';

var three = ' _ \n'+
            ' _|\n'+
            ' _|\n';

var createAccountFrom = function(str) {
  if (isValidAccountNumber(str)) {
    var AccountNumber = function() {

    };

    return new AccountNumber();
  } else {

    return undefined;
  }
};

var createNumber = function(str) {

  var lines = str.split('\n');

  var possibleValues = [zero,one,two,three];

  return {
    value: function() { 
      var lines = str.split('\n');

      for (var i=0;i<possibleValues.length;++i) {
        var matchingLines = possibleValues[i].split('\n');
        var match = true;
        for (var j=0;j<3;++j) {
          match = match && lines[j] === matchingLines[j];
        }

        if (match) {
          return i;
        }
      }
    }
  };
};


var isValidAccountNumber = function(str) {
  if (!str) {
    return false;
  }

  var lines = str.split('\n');
  if (lines.length !== 4) {
    return false;
  }

  return lines[0].length === 27 && 
         lines[1].length === 27 && 
         lines[2].length === 27;
};

describe('bank', function() {

  var useCase1 = ' _  _  _  _  _  _  _  _  _ \n' + 
                 '| || || || || || || || || |\n' +
                 '|_||_||_||_||_||_||_||_||_|\n' +
                 '';

  describe('input format', function() {
    it("consists of four lines", function() {
      var input = useCase1;
      assert.equal(4, input.split('\n').length);
    });

    it("each line consists of 27 characters", function() {
      var input = useCase1;
      var lines = input.split('\n');
      for(var i=0;i<3;++i) {
        assert.equal(27, lines[i].length);
      }
    });

    it("describes validility", function() {
      assert(isValidAccountNumber(useCase1));
    });
  });

  describe('account numbers', function() {
    it('is not created from a invalid input format', function() {
      assert.equal(undefined, createAccountFrom('12345'));
      assert.equal(undefined, createAccountFrom());
    });

    it('is created from a valid input format', function() {
      assert(createAccountFrom(useCase1));
    });
  });

  describe('number', function() {
    it('recognizes all numbers', function() {
      var nums = [zero,one,two,three,four,five,six,seven,eight,nine];
      for (var i=0;i<nums.length;++i) {
        assert.equal(i, createNumber(nums[i]).value());
      }
    });
  });
});
