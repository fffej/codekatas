"use strict";

var assert = require('assert');

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
  });

  describe('account numbers', function() {
    it('is not created from a invalid input format', function() {
      assert.equal(undefined, createAccountFrom('12345'));
      assert.equal(undefined, createAccountFrom());
    });
  });
});
