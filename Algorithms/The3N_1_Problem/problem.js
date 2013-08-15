"use strict";
var assert = require('assert');

describe('3n + 1', function() {
  describe('basics', function() {
    it('returns all values', function() {
      assert.deepEquals(
        [22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1],
        basic(22)
      );
    });
  });
});
