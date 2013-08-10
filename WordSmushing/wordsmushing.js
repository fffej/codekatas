"use strict";

var assert = require('assert');

var smush2 = function(a,b) {
  if (b.indexOf(a) === 0) {
    return b;
  } else {
    return a[0] + smush2(a.substr(1), b);
  }
};

describe('word smushing', function() {
  describe('two work smushing', function() {
    it('should smush two equal words', function() {
      var smushed = smush2('to','to');
      assert.equal('to', smushed);
    });

    it('should smush two letter words', function() {
      var smushed = smush2('it', 'to');
      assert.equal('ito', smushed);
    });

    it('should be conjunction  when doesn\'t smush', function() {
      var unsmushed = smush2('be', 'at');
      assert.equal('beat', unsmushed);
    });
  });

  describe('three word smushing', function() {
    it('should work for minimal example', function() {
      var smushed = smush('minutes','testing', 'ginger');

      assert.equal('minutestinginger', smushed);
    });
  });
});
