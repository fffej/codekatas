"use strict";

var assert = require('assert');

var canSmush = function(a,b) {
  return (a[a.length-1] === b[0]);
};



describe('word smushing', function() {
  describe('can smush', function() {
    it('should', function() {
      assert(canSmush('it','to'));
    });

    it('shouldn\'t', function() {
      assert(!canSmush('be','to'));
    });
  });

  describe('smush together', function() {
    it('joins together when 1 character overlaps', function() {
      var smushed = smush('it', 'to');
      assert.equal('ito', smushed);
    });
  });
});