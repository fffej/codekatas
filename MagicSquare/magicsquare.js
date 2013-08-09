"use strict";

var assert = require('assert');

// Canned fisher yates shuffle from bost.ocks.org/mike/shuffle
var shuffle = function(array) {
  var m = array.length, t, i;

  // While there remain elements to shuffle¡­
  while (m) {

    // Pick a remaining element¡­
    i = Math.floor(Math.random() * m--);

    // And swap it with the current element.
    t = array[m];
    array[m] = array[i];
    array[i] = t;
  }

  return array;
};

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

var createElementsOfGrid = function(size) {
  var elements = [];
  for (var i=1;i<size*size;++i) {
    elements.push(i);
  }
  return elements;
};

var MagicSquare = function(size) {

  var UP = 1, LEFT = 2, RIGHT = 3, DOWN = 4;

  var isBlank = function(x,y) { return x === (size-1) && y === (size-1); };

  var elements = createElementsOfGrid(size);
  var fill = function(x,y) { var n = elements.shift(); return n; };

  var grid = fillGrid(size, fill, isBlank);


  var onNearEdge = function(i) { return (i === size-1); };
  var onFarEdge =  function(i) { return i === 0; };

  return {
    
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

    shuffle: function() {
      var blankX = (Math.random() * size)|0;
      var blankY = (Math.random() * size)|0;
 
      var isBlank = function(x,y) { return x === blankX && y === blankY; };
      shuffle(elements);

      grid = fillGrid(size, fill, isBlank);
    },
    
    solved: function() {
      var counter = 1;
      for (var i=0;i<size;++i) {
        for (var j=0;j<size;++j) {
          if (i === (size-1) && j === (size-1)) {
            return grid[i][j] === '*';
          }
          if (grid[i][j] !== counter) {
            return false;
          }
          counter++;
        }
      }
      return true;
    },


    validMoves: function() {
      var validMoves = [];

      for (var i=0;i<size;++i) {
        for (var j=0;j<size;++j) {
          if (grid[i][j] === '*') {
            if (onFarEdge(j)) {
              validMoves.push(UP);
            }

            if (onNearEdge(j)) {
              validMoves.push(DOWN); 
            }

            if (onNearEdge(i)) {
              validMoves.push(LEFT);
            }

            if (onFarEdge(i)) {
              validMoves.push(RIGHT);
            }
            break;
          }
        }
      }
      return validMoves;
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

  it('can make a move', function() {
    var sq = new MagicSquare(3);

    assert.equal(2, sq.validMoves().length);
  });
});