"use strict";
var assert = require('assert');

var cellFrom = function(c) {
  return new Cell(c);
};

var createCellCollection = function(s) {
  var cells = [];

  for (var i=0;i<s.length;++i) {
    cells.push(cellFrom(s[i]));
  }

  return new CellCollection(cells);
};

var CellCollection = function(cells) {
  return {
    length: cells.length,

    at: function(i) {
      return cells[i];
    },

    validMove: function(idx, whoseMove) {

      // Non empty cells obviously aren't valid
      if (!cells[idx].isEmpty()) {
        return false;
      }

      var target = cellFrom(whoseMove);

      var oneEncountered = false;
      for (var i=idx-1;i>=0;i--) {
        if (cells[i].opposite(target)) {
          oneEncountered = true;
        }
        else if (cells[i].same(target)) {
           break;
        }
      }

      if (oneEncountered) {
        return true;
      }

      oneEncountered = false;
      for (var i=idx+1;i<cells.length;++i) {
        if (cells[i].opposite(target)) {
          oneEncountered = true;
        }  
        else if (cells[i].same(target)) {
          break;
        }
      }

      return oneEncountered;
    }
  };
};

var createBoard = function(s) {
  var lines = s.split('\n');

  var rows = [];

  for (var i=0;i<lines.length;++i) {
    rows.push(createCellCollection(lines[i]));
  }

  return new Board(rows,lines[8][0]);
};

var Board = function(rows,t) {

  var _rowData = rows; 

  return {
    rows: 8,
    cols: 8,
    at: function(r,c) {
      return _rowData[r].at(c)
    },
    turn: function() {
      return cellFrom(t);
    }
  };
};

var Cell = function(c) {
  return {
    isBlack: function() { return c === 'B'; },
    isWhite: function() { return c === 'W'; },
    isEmpty: function() { return c === '.'; },

    opposite: function(other) {
      if (this.isBlack() && other.isWhite()) { return true; }
      if (this.isWhite() && other.isBlack()) { return true; }

      return false;
    },

    same: function(other) {
      var isBlack = this.isBlack() && other.isBlack();
      var isWhite = this.isWhite() && other.isWhite();

      return isBlack || isWhite;
    }
  };
};

describe('reversi', function() {

  it('basic predicates', function() {

    var bCell = cellFrom('B');
    var wCell = cellFrom('W');
    var eCell = cellFrom('.');

    assert(bCell.isBlack());
    assert(wCell.isWhite());
    assert(eCell.isEmpty());
  });

  it('knows opposites', function() {
    var black = cellFrom('B');
    var white = cellFrom('W');

    assert.equal(true, black.opposite(white));
    assert.equal(true, white.opposite(black));
  });

  it('knows same', function() {
    var black = cellFrom('B');
    var white = cellFrom('W');

    assert(black.same(black));
    assert.equal(false, black.same(white));
  });

  describe('collections of cells', function() {
    it('can create', function() {
      var cellCollection = createCellCollection('........');
      assert.equal(8, cellCollection.length);
    });

    it('simple no valid move', function() {
      var cellCollection = createCellCollection('...');
      assert.equal(false, cellCollection.validMove(0, 'B'));
    });

    it('can test for a simple capture', function() {
      var cellCollection = createCellCollection('BW.');
      assert(cellCollection.validMove(2,'B'));

      cellCollection = createCellCollection('.WB');
      assert(cellCollection.validMove(0,'B'));
    });
  });

  it('has a concept of a board', function() {
    var board = createBoard(exampleInput);

    assert.equal(8,board.rows);
    assert.equal(8,board.cols);

    assert(board.at(3,3).isBlack());
    assert(board.at(3,4).isWhite());

    assert(board.turn().isBlack());
  });
 
  var exampleInput = '........\n' +
                     '........\n' +
                     '........\n' +
                     '...BW...\n' +
                     '...WB...\n' +
                     '........\n' +
                     '........\n' +
                     '........\n' +
                     'B\n';

  var exampleOutput = '........\n' +
                      '........\n' +
                      '....0...\n' +
                      '...BW0..\n' +
                      '..0WB...\n' +
                      '...0....\n' +
                      '........\n' +
                      '........\n' +
                      'B\n';

  var drive = function(input) {
    var board = createBoard(input);
    return 'not done yet';
  };

  describe('acceptance test', function() {
    it('input/output should match expectations', function() {
      assert.equal(exampleOutput, drive(exampleInput));
    });
  });
});
