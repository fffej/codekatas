"use strict";
var assert = require('assert');

var sub = function(x,y) {
  return y.indexOf(x) !== -1;
};

describe('word smushing', function() {
  describe('substring', function() {
    it('x is a substring of y', function() {
      assert(sub('bana', 'banana'));
      assert(sub('banana', 'banana'));
      assert(sub('ana', 'banana'));
    });

    it('x is not a substring of y', function() {
      assert(!sub('jeff', 'john'));
    });
  });
});
