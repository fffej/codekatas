"use strict";
var assert = require('assert');

var cellFrom = function(c) {
  return new Cell(c);
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
