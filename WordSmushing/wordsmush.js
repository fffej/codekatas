"use strict";

var assert = require('assert');

describe('word smush', function() {
  describe('smush two works', function() {
    it('should concatenate words with no overlaps', function() {
      assert.equal("catdog", smush2("cat", "dog"));
    });
  });
});