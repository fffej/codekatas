"use strict";

var assert = require('assert');


var largestString = function(words) {
  if (words && words.length > 1) {
    return words.reduce(function(x,y) {
      return Math.max(x,y.length); 
    }, words[0].length);
  }
};

var eliminateSubstrings = function(words) {
  var rest = [];
  var n = words.length;

  for (var i=0;i<n;++i) {
    var subString = false;
    for (var j=0;j<n;++j) {
      if (i == j) { 
        continue;
      }

      if (words[j].indexOf(words[i]) !== -1) {
        subString = true;
      }
    }

    if (!subString) {
      rest.push(words[i]);
    }
  }


  return rest;
};

describe('words smushing', function() {
  describe('largest string', function() {
    it('is not defined for 0 length arrays', function() {
      assert(!largestString([]));
    });

    it('returns the length of the longest string', function() {
      assert.equal(5, largestString(['a','b','bc','defgh']));
    });
  });

  describe('eliminate wholly contains substrings', function() {
    it('duplicates are eliminated', function() {
      assert.equal(1, eliminateSubstrings(['test','testing']).length);
    });
  });
});
