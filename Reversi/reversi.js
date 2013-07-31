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

    validMove: function(idx) {
      return false;
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

    opposite: function() {
      if (this.isBlack()) { return new Cell('W'); }
      if (this.isWhite()) { return new Cell('B'); }
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
    assert(black.opposite().isWhite());

    var white = cellFrom('W');
    assert(white.opposite().isBlack());
  });

  describe('collections of cells', function() {
    it('can create', function() {
      var cellCollection = createCellCollection('........');
      assert.equal(8, cellCollection.length);
    });

    it('simple no valid move', function() {
      var cellCollection = createCellCollection('...');
      assert.equal(false, cellCollection.validMove(0));
    });

    it('can test for a simple capture', function() {
      var cellCollection = createCellCollection('BW.');
      assert(cellCollection.validMove(2,'B'));
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
