"use strict";

var assert = require('assert');

var smush = function(a,b) {

};

describe('word smushing', function() {
  describe('two work smushing', function() {
    it('should smush', function() {
      var smushed = smush('to','to');
      assert.equal('to', smushed);
    });
  });
});
