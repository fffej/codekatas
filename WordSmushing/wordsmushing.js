"use strict";
var assert = require('assert');

describe('word smushing', function() {
  describe('substring', function() {
    it('y is a substring of x', function() {
      assert(sub('banana', 'bana'));
      assert(sub('banana', 'banana'));
      assert(sub('banana', 'ana'));
    });
  });
});
