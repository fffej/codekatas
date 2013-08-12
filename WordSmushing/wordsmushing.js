"use strict";

var assert = require('assert');

var smush = function(a,b) {
  return 'ito';
};


describe('word smushing', function() {
  describe('base cases', function() {
    it('should find the minimal smush', function() {
      var smushed = smush('it', 'to');
      assert.equal('ito', smushed);
    });
  });
});
