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

var smush = function(x,y) {
  var n = overlap(x,y);

  return x + y.substr(n);
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

  worthyWords.sort();
  return worthyWords;
};

var createTable = function(words) {
  var n = words.length;
  var table = new Array(n);
  for (var i=0;i<n;++i) {
    table[i] = new Array(n);
  }

  for (var i=0;i<n;++i) {
    for (var j=0;j<n;++j) {
      if (i === j) {
        table[i][j] = '';
        continue;
      }
 
      table[i][j] = smush(words[i],words[j]);
    }
  }

  return table;
};

var getMin = function(ret,c) {
  if (ret.length === 0 || ret.length > c.length) {
    return c;
  }

  // Arbitrary pick
  if (ret.length === c.length && ret > c) {
    return c;
  }

  return ret;
};

var solve = function(table, d, words,state,tail) {
  if (state === 0) {
    return "";
  }

  if (state === (1 << tail)) {
    return words[tail];
  }

  if (d[state][tail].length !== 0) {
    return d[state][tail];
  }

  var ret = '';
  var n = words.length;

  for (var i=0;i<n;++i) {
    var bitset = state & (1 << i);
    if (bitset && i !== tail) {
      var bitToTurnOff = ~(1 << tail);

      ret = getMin(
        ret,
        solve(
          table,
          d,
          words,
          state & (~(1 << tail)),
          i
         ) + table[i][tail]);
    }
  }

  d[state][tail] = ret;
  return ret;
};

var createJumpTable = function(n) {
  var d = new Array(1 << n);
  for (var i=0;i<(1 << n);++i) {
    d[i] = new Array(n);
    for (var j=0;j<n;++j) {
      d[i][j] = '';
    }
  }

  return d;
};

var joinWords = function(words) {
  // Filter out those words that get solved by free
  words = worthyWords(words);

  var n = words.length;
  var table = createTable(words);
  var d = createJumpTable(n);

  // Initial solution table
  var ret = '';
  for (var i=0;i<n;++i) {
    ret = getMin(ret, solve(table, d, words, (1 << n) - 1, i));
  }

  return ret;
};

describe('word smushing', function() {

  describe('joining words', function() {
   it('should work for two words', function() {
     assert.equal('abcdef', joinWords(['abc','def','ghi']));
   });
  });

  describe('table', function() {
    it('makes sense', function() {
      var table = createTable(['german','ginger','testing']);
      assert.deepEqual([
          ['',   'germanginger', 'germantesting'],
          ['gingerman',   '',    'gingertesting'],
          ['testingerman','testinginger', '']
         ], table);
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

    it('smushes', function() {
      assert.equal('jeffish', smush('jeff', 'ffish'));
      assert.equal('abcd', smush('ab','cd'));
    });
  });
});

