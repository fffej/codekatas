"use strict";

var assert = require('assert');

var MagicSquare = function(size) {

  var grid = [];
  var counter = 1;

  for (var i=0;i<size;++i) {
    var row = [];
    for (var j=0;j<size;++j) {
      row.push(counter++);
    }
    grid.push(row);
  }

  grid[size-1][size-1] = '*';

  return {
    shuffle: function() {},
    display: function() {
      var s = '';
      for (var i=0;i<size;++i) {
        for (var j=0;j<size;++j) {
          s += grid[i][j];
        }
        s+='\n';
      }
      return s;
    },
    solved: function() {
      var counter = 1;
      for (var i=0;i<size;++i) {
        for (var j=0;j<size;++j) {
          if (grid[i][j] !== counter) {
            return false;
          }
        }
      }
      return true;
    }
  };
};

describe('magic square', function() {
  it('exists', function() {
    assert(new MagicSquare(3));
  });

  it('can be shuffled', function() {
    assert(new MagicSquare(3).shuffle);
  });

  it('can be display', function() {
    assert.equal('123\n456\n78*\n',new MagicSquare(3).display());
  });

  it('is solved without shuffling', function() {
    var square = new MagicSquare(3);
    assert(square.solved());
    
    // Small chance the shuffle could magically come back?
    square.shuffle();
    assert(!square.solved());
  });
});