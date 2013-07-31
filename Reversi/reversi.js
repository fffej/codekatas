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

  return cells;
};

var createBoard = function(s) {
  var lines = s.split('\n');

  var rows = [];

  for (var i=0;i<lines.length;++i) {
    rows.push(createCellCollection(lines[i]));
  }

  return rows;
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

  it('can deal with collections', function() {
    var cellCollection = createCellCollection('........');
    assert.equal(8, cellCollection.length);
  });

  it('has a concept of a board', function() {
    var board = createBoard(exampleInput);
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
    return 'not done yet';
  };

  describe('acceptance test', function() {
    it('input/output should match expectations', function() {
      assert.equal(exampleOutput, drive(exampleInput));
    });
  });
});
