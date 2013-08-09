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

var smushIndex = function(a,b) {
  if (!canSmush(a,b)) throw Error('unsmushable');

  if (b.indexOf(a) === 0) {
    return 0;
  } else {
    return 1 + smushIndex(a.substr(1),b);
  }
};

var smush = function(a,b) {
  if (!canSmush(a,b)) throw Error('unsmushable');
  return a.substr(0,smushIndex(a,b)) + b;
};

var multiSmush = function() {
  if (arguments.length === 1) {
    return arguments[0];
  } else {
  }
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

    it('should smush multiple overlaps', function() {
      var smushed = smush('bea', 'ear');
      assert.equal('bear', smushed);
    });

    it('should perform with test data', function() {
      var smushed = smush('testing', 'ginger');
      assert.equal('testinginger',smushed);
    });
  });

  describe('multiple smushings', function() {
    it('supports multiple arguments', function() {
      var smushed = multiSmush('eff');
      assert.equal('eff', smushed);
    });

    it('can join together', function() {
      var smushed = multiSmush('minutes', 'testing', 'ginger');
 
      assert.equal('minutestinginger', smushed);
    });
  });
});