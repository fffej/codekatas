"use strict";
var assert = require('assert');

describe('haiku', function() {
  describe('syllables', function() {
    it('should be able to count syllables', function() {
      assert.equal(1, syllableCount('bat'));
    });
  });
});
