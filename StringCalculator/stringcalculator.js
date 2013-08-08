"use strict";

var assert = require('assert');

var StringCalculator = function() {
  return {
    add: function(s) {
      if (s === '') {
        return 0;
      }
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
  });
});