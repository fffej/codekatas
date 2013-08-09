"use strict";

var assert = require('assert');

var smush = function(a,b) {
  return (a[a.length-1] === b[0]);
};

describe('word smushing', function() {
  describe('can smush', function() {
    it('should', function() {
      assert(smush('it','to'));
    });

    it('shouldn\'t', function() {
      assert(!smush('be','to'));
    });
  });
});