"use strict";

var assert = require('assert');

var canSmush = function(a,b) {
  if (a === '') { return false; }

  if (b.indexOf(a) !== -1) {
    return true;
  } else {
    return canSmush(a.substr(1), b);
  }
};

var smush = function(a,b) {
  if (!canSmush(a,b)) throw Error('unsmushable');

  return a + b.substr(1);
};

describe('word smushing', function() {
  describe('can smush', function() {
    it('should deal with single character smushes', function() {
      assert(canSmush('it','to'));
    });

    it('should deal with multiple overlaps', function() {
      assert(canSmush('bea', 'ear'));
    });

    it('shouldn\'t', function() {
      assert(!canSmush('be','to'));
    });
  });

  describe('smush together', function() {
    it('wont smush unsmushable', function() {
      assert.throws(function() { smush('be','to'); });
    });

    it('joins together when 1 character overlaps', function() {
      var smushed = smush('it', 'to');
      assert.equal('ito', smushed);
    });
  });
});