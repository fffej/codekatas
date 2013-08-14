"use strict";
var assert = require('assert');

var sub = function(x,y) {
  return y.indexOf(x) !== -1;
};

var overlap = function(x, y) {
  var xn = x.length;
  var yn = y.length;
  var max = Math.min(xn, yn);
  for (var i=max-1;i>=1;i--) {
    if (x.substr(xn - i, i) === y.substr(0,i)) {
      return i;
    }
  }

  return 0;
};


var worthyWords = function(words) {
  var n = words.length;

  var hash = new Array(n);

  for (var i=0;i<n;++i) {
    hash[i] = false;
    for (var j=0;j<n;j++) {
      if (i !== j) {
        if (sub(words[i], words[j])) {
          if (words[i] !== words[j] || i < j) {
            hash[i] = true;
          }
        }
      }
    }
  }

  var worthyWords = [];
  for (var i=0;i<n;++i) {
    if (!hash[i]) {
      worthyWords.push(words[i]);
    }
  }

  return worthyWords;
};


var overlapTable = function(words) {
  var n = words.length;
  var a = new Array(n);
  for (var i=0;i<n;++i) {
    a[i] = new Array(n);
    for (var j=0;j<n;++j) {
      a[i][j] = overlap(words[i], words[j]);
    }
  }

  return a;
};

describe('word smushing', function() {

  describe('table of overlaps', function() {
    it('should be identity', function() {
      var words = ['aa','bb','cc'];
      assert.deepEqual([ [1,0,0],
                         [0,1,0],
                         [0,0,1] ], overlapTable(words));
    });

    it('should make sense', function() {
      var words = [
        'testing',
        'ginger', 
        'german',
        'minutes'
      ];
      assert.deepEqual([ [0,1,1,0],
                         [0,0,3,0],
                         [0,0,0,0],
                         [3,0,0,0] ], overlapTable(words));
    });
  });

  describe('words worth considering', function() {
    it('should only consider words that are important', function() {
      var worthy = worthyWords(['baab','aa', 'cc']);
      assert.deepEqual(['baab','cc'], worthy);
    });
  });

  describe('substring', function() {
    it('x is a substring of y', function() {
      assert(sub('bana', 'banana'));
      assert(sub('banana', 'banana'));
      assert(sub('ana', 'banana'));
    });

    it('x is not a substring of y', function() {
      assert(!sub('jeff', 'john'));
    });
  });

  describe('overlap', function() {
    it ('overlaps', function() {
      assert.equal(2, overlap('jeff', 'ffish'));
      assert.equal(1, overlap('testing', 'ginger'));
      assert.equal(3, overlap('testing', 'ingot'));
    });

    it('doesn\'t', function() {
      assert.equal(0, overlap('jef', 'john'));
    });
  });
});
