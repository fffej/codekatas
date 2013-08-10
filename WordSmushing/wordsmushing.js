"use strict";

var assert = require('assert');

var smush = function(a,b) {
  if (a === b) {
    return a;
  }
};

describe('word smushing', function() {
  describe('two work smushing', function() {
    it('should smush two equal words', function() {
      var smushed = smush('to','to');
      assert.equal('to', smushed);
    });

    it('should smush two letter words', function() {
      var smushed = smush('it', 'to');
      assert.equal('ito', smushed);
    });
  });
});
