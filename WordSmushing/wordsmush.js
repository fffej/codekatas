"use strict";

var assert = require('assert');

var smush2 = function(a,b) {
  if (a === '') { 
    return b;
  } else {
    return a[0] + smush2(a.substr(1), b);
  }
};

describe('word smush', function() {
  describe('smush two works', function() {
    it('should concatenate words with no overlaps', function() {
      assert.equal("catdog", smush2("cat", "dog"));
    });

    it('should skip adjacent characters', function() {
      assert.equal('find', smush2('fin', 'ind'));
    });
  });
});