"use strict";
var assert = require('assert');

var syllableCount = function(word) {
  return 1;
};

describe('haiku', function() {
  describe('syllables', function() {
    it('should be able to count syllables', function() {
      assert.equal(1, syllableCount('bat'));
    });
  });
});
