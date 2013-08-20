"use strict";

var assert = require('assert');

var smush2 = function(a,b) {
  if (b.indexOf(a) === 0) {
    return b;
  } 

  return a[0] + smush2(a.substr(1), b);
};

var smush = function() {
  if (arguments.length === 2) {
    return smush2(arguments[0], arguments[1]);
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

  describe('minimal solution', function() {
    it('should find the minimal solution for two words', function() {
      assert.equal('boing', smush('ing', 'bo'));
    });
  });
});