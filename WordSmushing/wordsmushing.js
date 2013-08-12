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

var bestSmush = function(words) {
  var n = words.length;
  var best = { score: -1 };

  for (var i=0;i<n;++i) {
    for (var j=0;j<n;++j) {
      if (i === j) {
        continue;
      }

      var len = (words[i] + words[j]).length;
      var smushed = smush(words[i], words[j]);
      var score = len - smushed.length;
      if (score > best.score) {
        best.score = score;
        best.result = smushed;
        best.arg1 = i;
        best.arg2 = j;
      }
    }
  }
  
  return best;
};

var getRest = function(words, n, m) {
  var rest = [];
  for (var i=0;i<words.length;++i) {
    if (i ===n || i === m) {
      continue;
    }

    rest.push(words[i]);
  }

  return rest;
};

var smushWords = function(words) {
  // Will a greedy match work?
  var n = words.length;
  if (n === 2) {
    return smush(words[0], words[1]);
  } else {

    var best = bestSmush(words);

    var rest = getRest(words, best.arg1, best.arg2);
    rest.push(best.result);

    return smushWords(rest);
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
      var smushed = smushWords(['testing','ginger']);

      assert.equal('testinginger', smushed);
    });

    it('should choose first pair based on overlap', function() {
      var smushed = smushWords(['orange','gherkin','gerg']);
      assert.equal(0, smushed.indexOf('orangerg'));
    });

    it('works greedily', function() {
      var smushed = smushWords(words);
      assert.equal('minutestingingerman', smushed);
    });
  });
});
