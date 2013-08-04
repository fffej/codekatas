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

  return {
    value: function() { 
      if (lines[0] === ' _ ' && lines[1] !== ' _|') {
        return 0;
      } else if (lines[1] === '  |') {
        return 1;
      } else if (lines[1] === ' _|') {
        return 2;
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
    it('zero string', function() {
      assert.equal(0, createNumber(zero).value());
    });

    it('one string', function() {
       assert.equal(1, createNumber(one).value());
    });

    it('two string', function() {
      assert.equal(2, createNumber(two).value());
    });

    it ('three string', function() {
      assert.equal(3, createNumber(three).value());
    });
  });
});