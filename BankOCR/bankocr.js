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

var four = '   \n' +
           '|_|\n' +
           '  |\n';

var five = ' _ \n' +
           '|_ \n' +
           ' _|\n';

var six = ' _ \n' +
          '|_ \n' +
          '|_|\n';

var seven = ' _ \n' +
            '  |\n' +
            '  |\n';

var eight = ' _ \n' +
            '|_|\n' +
            '|_|\n';

var nine = ' _ \n' +
           '|_|\n' +
           '  |\n';

var createAccountFrom = function(str) {
  if (isValidAccountNumber(str)) {

    var lines = str.split('\n');
    var numbers = [];

    for (var start=0;start<27;start+=3) {
      var numberInput = lines[0].substr(start,3) + '\n' +
                        lines[1].substr(start,3) + '\n' +
                        lines[2].substr(start,3) + '\n';

      numbers.push(createNumber(numberInput));
    }

    var AccountNumber = function() {
      return {
        toString: function() {
          var s = '';
          for (var i=0;i<9;++i) {
            s += '' + numbers[i].value();
          }
          return s;
        }
      }
    };

    return new AccountNumber();
  } else {

    return undefined;
  }
};

var createNumber = function(str) {

  var lines = str.split('\n');

  var possibleValues = [zero,one,two,
                       three,four,five,six,
                       seven,eight,nine];

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

  var useCase2 = '    _  _     _  _  _  _  _ \n' +
                 '  | _| _||_||_ |_   ||_||_|\n' +
                 '  ||_  _|  | _||_|  ||_| _|\n' +
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

    it('can extract numbers', function() {
      var allZeros = createAccountFrom(useCase1);
      assert.equal('000000000', allZeros.toString());

      var oneToNine = createAccountFrom(useCase2);
      assert.equal('123456789', oneToNine.toString());
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
