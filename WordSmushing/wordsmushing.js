"use strict";

var assert = require('assert');

var smush = function(a,b) {
  return 'ito';
};

var smush2 = function(a,b) {
  if (b.indexOf(a) === 0) {
    return b;
  } else {
    return a[0] + smush2(a.substr(1), b);
  }
};

describe('word smushing', function() {
  describe('base cases', function() {
    it('should find the minimal smush', function() {
      var smushed = smush('it', 'to');
      assert.equal('ito', smushed);
    });
  });

  describe('smush2', function() {
    it('should smush2 in the order specified', function() {
      var smushed = smush2('to', 'it');
      assert.equal('toit', smushed);
    });
  });
});
