"use strict";

var assert = require('assert');

var createAccountFrom = function(str) {
  if (isValidAccountNumber(str)) {

  } else {
    return undefined;
  }
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
  });
});
