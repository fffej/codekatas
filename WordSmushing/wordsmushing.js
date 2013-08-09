"use strict";

var assert = require('assert');

var smush = function(a,b) {
  return true;
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