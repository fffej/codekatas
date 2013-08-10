"use strict";

var assert = require('assert');

var smush = function(a,b) {

};

describe('word smushing', function() {
  describe('two work smushing', function() {
    it('should smush', function() {
      var smushed = smush('it','to');
      assert.equal('ito', smushed);
    });
  });
});
