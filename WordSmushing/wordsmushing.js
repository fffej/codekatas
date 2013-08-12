"use strict";

var assert = require('assert');

var smush = function(a,b) {
  var c = smush2(a,b);
  var d = smush2(b,a);

  return c.length > d.length ? d : c;
};

var smush2 = function(a,b) {
  if (b.indexOf(a) === 0) {
    return b;
  } else {
    return a[0] + smush2(a.substr(1), b);
  }
};

var smushWords = function(words) {
  // Will a greedy match work?
  var n = words.length;
  if (n === 2) {
    return smush(words[0], words[1]);
  } else {
    throw Error('writing too much code');
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

    it('aligns smushes', function() {
      var smushed = smush2('it','to');
      assert.equal('ito', smushed);
    });
  });

  describe('small values', function() {
    var words = ['testing', 'ginger', 'german', 'minutes'];

    it('should find a minimal solution', function() {
      var smushed = smushWords(words);

      assert.equal('minutestingingerman', smushed);
    });
  });
});
