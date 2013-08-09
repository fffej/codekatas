"use strict";

var assert = require('assert');

var MovesEnum = {
  Up: 0,
  Down: 1,
  Left: 2,
  Right: 3
};

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

  var isBlank = function(x,y) { return x === (size-1) && y === (size-1); };

  var elements = createElementsOfGrid(size);
  var fill = function(x,y) { var n = elements.shift(); return n; };

  var grid = fillGrid(size, fill, isBlank);


  var onNearEdge = function(i) { return (i === size-1); };
  var onFarEdge =  function(i) { return i === 0; };

  var findBlank = function() {
    for (var i=0;i<size;++i) {
      for (var j=0;j<size;++j) {
        if (grid[i][j] === '*') {
          return { x: j, y: i };
        }
      }
    }
  };

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

    makeMove: function(move) {
      var blankPos = findBlank();

      switch (move) {
        case MovesEnum.Up:
          this.swap(blankPos, { x: blankPos.x, y: blankPos.y - 1});
          break;
        case MovesEnum.Down:
          this.swap(blankPos, { x: blankPos.x, y: blankPos.y + 1});
          break;
        case MovesEnum.Left:
          this.swap(blankPos, { x: blankPos.x - 1, y: blankPos.y});
	  break;
        case MovesEnum.Right:
          this.swap(blankPos, { x: blankPos.x + 1, y: blankPos.y});
          break;
      }
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

    swap: function(p, q) {
      var temp = grid[p.y][p.x];
      grid[p.y][p.x] = grid[q.y][q.x];
      grid[q.y][q.x] = temp;
    },

    validMoves: function() {
      var validMoves = [];

      for (var i=0;i<size;++i) {
        for (var j=0;j<size;++j) {
          if (grid[i][j] === '*') {
            if (onNearEdge(j)) {
              validMoves.push(MovesEnum.Up);
            }

            if (onFarEdge(j)) {
              validMoves.push(MovesEnum.Down); 
            }

            if (onFarEdge(i)) {
              validMoves.push(MovesEnum.Left);
            }

            if (onNearEdge(i)) {
              validMoves.push(MovesEnum.Right);
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

  it('can define valid moves', function() {
    var sq = new MagicSquare(3);

    assert.equal(2, sq.validMoves().length);
  });

  it('moves make sense', function() {
    var sq = new MagicSquare(3);
  
    assert(sq.validMoves().indexOf(MovesEnum.Up) !== -1);
    assert(sq.validMoves().indexOf(MovesEnum.Right) !== -1);
  });

  it('can make a move up', function() {
    var sq = new MagicSquare(3);
    sq.makeMove(MovesEnum.Up);
    assert.equal('123\n45*\n786\n', sq.display());
  });

  it('can make a move left', function() {
    var sq = new MagicSquare(3);
    sq.makeMove(MovesEnum.Left);
    assert.equal('123\n456\n7*8\n', sq.display());
  });
});

// What did I learn (or remphasize)
// - How to better fake enums in JavaScript
// - Do EXACTLY ONE THING for each test, otherwise you get in a pickle
// - Be consistent, cos the type system (capitalization of Enums for example)