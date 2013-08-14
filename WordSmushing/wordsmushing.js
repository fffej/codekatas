"use strict";
var assert = require('assert');

var sub = function(x,y) {
  return y.indexOf(x) !== -1;
};

var overlap = function(x, y) {
  var xn = x.length;
  var yn = y.length;
  var max = Math.min(xn, yn);
  for (var i=max-1;i>=1;i--) {
    if (x.substr(xn - i, i) === y.substr(0,i)) {
      return i;
    }
  }

  return 0;
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

  describe('overlap', function() {
    it ('overlaps', function() {
      assert.equal(2, overlap('jeff', 'ffish'));
    });
  });
});
