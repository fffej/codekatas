"use strict";

var assert = require('assert');

var DEAD = 0;
var LIVE = 1;

var sampleInput = "4 8\n........\n....*...\n...**...\n........\n";
var sampleOutput = "4 8\n........\n...**...\n...**...\n........\n";

var Game = function(r,c) {
  var game = { 
      rows: r, 
      cols: c,
      neighbours: function(c,r) {
        var n = 0;
        for (var i=-1;i<=1;++i) {
          for (var j=-1;j<=1;++j) {
            if (i === 0 && j === 0) {
              continue;
            }

            if (this[c+j] && this[c+j][r+i] === LIVE) {
              ++n;
            }
          }
        }
        return n;
      },
      to_string: function() {
        var s = this.rows + ' ' + this.cols + '\n';
        for (var i=0;i<this.rows;++i) {
          for (var j=0;j<this.cols;++j) {
            s+= (this[j][i] === LIVE ? '*' : '.');
          }
          s+='\n';
        }
        return s;
      },
      iterate: function() {
        var newGame = new Game(this.rows, this.cols);

        for (var i=0;i<this.rows;++i) {
          for (var j=0;j<this.cols;++j) {
            var state = this[j][i];
            var neighbourCount = this.neighbours(j,i);

            if (newGame[j] === undefined) { 
              newGame[j] = []; 
            }
            newGame[j][i] = cellState(state, neighbourCount);
          }
        }

        return newGame;
      }
  };
  return game;
};

var createGrid = function(s) {
  var lines = s.split('\n');
  var rc = lines[0].split(' ');
  var rows = rc[0] | 0; // 4
  var cols = rc[1] | 0; // 8

  var game = new Game(rows,cols);  

  for (var i=0;i<rows;++i) {
    for (var j=0;j<cols;++j) {
      var v = lines[1+i][j];
      if (game[j] === undefined) { game[j] = []; }
      game[j][i] = v === '*' ? LIVE : DEAD;
    }
  } 

  return game;
};

var cellState = function(state,neighbours) {
  if (neighbours < 2 && state === LIVE) {
    return DEAD;
  }
  else if (neighbours > 3 && state === LIVE) {
    return DEAD;
  }
  else if ((neighbours === 2 || neighbours === 3) && state === LIVE) {
    return LIVE;
  }
  else if (neighbours === 3 && state === DEAD) {
    return LIVE;
  }
  else {
    return DEAD;
  }
};

describe('game of life', function() {
  describe('cell', function() {
    it('dies with underpopulation', function() {
      assert.equal(DEAD, cellState(LIVE,1));
    });

    it('dies with overcrowding', function() {
      assert.equal(DEAD, cellState(LIVE,4));
    });

    it('maintains live if 2 or three neighbours', function() {
      assert.equal(LIVE, cellState(LIVE,2));
      assert.equal(LIVE, cellState(LIVE,3));
    });

    it('becomes alive', function() {
      assert.equal(LIVE, cellState(DEAD,3));
    });

    it('is fully defined', function() {
      for (var i=0;i<=8;++i) {
        assert(undefined !== cellState(DEAD,i));
        assert(undefined !== cellState(LIVE,i));
      }
    });
  });

  describe('grid', function() {
    it('should read input', function() {
      var grid = createGrid(sampleInput);

      assert.equal(4, grid.rows);

      assert.equal(DEAD, grid[0][0]);
      assert.equal(LIVE, grid[4][1]);
      assert.equal(LIVE, grid[3][2]);
      assert.equal(LIVE, grid[4][2]);
    });

    it('should be able to get the number of live neighbours', function() {
      var grid = createGrid(sampleInput);

      assert.equal(0, grid.neighbours(0,0));
      assert.equal(2, grid.neighbours(3,2));

      grid = createGrid('2 2\n..\n..');
      assert.equal(0, grid.neighbours(0,0));
      assert.equal(0, grid.neighbours(1,0));
      assert.equal(0, grid.neighbours(0,1));
      assert.equal(0, grid.neighbours(1,1));

      grid = createGrid('2 2\n**\n**');
      assert.equal(3, grid.neighbours(0,0));
      assert.equal(3, grid.neighbours(1,0));
      assert.equal(3, grid.neighbours(0,1));
      assert.equal(3, grid.neighbours(1,1));
    });

    it('should have a sensible printed representation', function() {
      var grid = createGrid('1 1\n.');
      assert.equal('1 1\n.\n', grid.to_string());

      grid = createGrid(sampleInput);
      assert.equal(sampleInput, grid.to_string());
    });

    it('can iterate', function() {
      var grid = createGrid('2 2\n..\n.*\n');   
      assert.equal('2 2\n..\n..\n', grid.iterate().to_string());
    });

    it('produces right output for sample input', function() {
      var grid = createGrid(sampleInput);
      assert.equal(sampleOutput, grid.iterate().to_string());
    });
  });
});

// Retrospective
// - Painful
// - x[1,2,3,4] doesn't do what you expect for an object indexer
//     This is because "," is an operator that evaluates arguments in turn
// - Getting rows and columns right is hard :)
// - Code is too big, not very clean.
