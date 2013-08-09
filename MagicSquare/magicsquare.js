"use strict";

var assert = require('assert');

var fillGrid = function(size, fill, isBlank) {
  var grid = [];
  for (var i=0;i<size;++i) {
    var row = [];
    for (var j=0;j<size;++j) {
      if (isBlank(i,j)) {
        row.push('*');
      } else {
        row.push(fill(i,j));
      }
    }
    grid.push(row);
  }
  return grid;
};

var MagicSquare = function(size) {
  var counter = 1;
  var fill = function(x,y) { return counter++; };

  var isBlank = function(x,y) { return x === (size-1) && y === (size-1); };

  var grid = fillGrid(size, fill, isBlank);

  return {
    shuffle: function() {

    },
    
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
          if (i === (size-1) && j === (size-1)) {
            return grid[i][j] === '*';
          }
          if (grid[i][j] !== counter) {
            console.log(i+','+j+','+counter);
            return false;
          }
          counter++;
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
  });
    
  it('is not solved after shuffling', function() {
    var square = new MagicSquare(3);

    // Small chance the shuffle could magically come back?
    square.shuffle();
    assert(!square.solved());
  });
});