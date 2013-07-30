"use strict";

var assert = require('assert');

var WHITE = 1;
var BLACK = 2;

var isBlank = function(c) { return c === '.'; };
var isBlack = function(c) { return c === 'B'; };
var isWhite = function(c) { return c === 'W'; };
var opposite = function(c) { return isBlack(c) ? 'W' : 'B'; };
var isOpposite = function(c,t) { return c === opposite(t); };

var capturableIndices = function(s, c) {

  var i = s.indexOf(c);
  if (-1 === i) {
    return [];
  }

  var validIndexes = [];

  while (i !== -1) {
    // Is there one after?
    i = s.indexOf(c,i + 1);

    for (var x = i + 1; x < s.length; ++x) {
      if (isBlank(s[x])) {
        validIndexes.push(x);
        break;
      }
    }
  }  

  return validIndexes;
};

var reversiFromString = function(s) {
  var lines = s.split('\n');

  return {
    turn: function() {
      return lines[8] === 'B' ? BLACK : WHITE;
    }
  }
};

describe('reversi', function() {

  var sampleInput = '........\n' +
                    '........\n' +
                    '........\n' +
                    '...BW...\n' +
                    '...WB...\n' +
                    '........\n' +
                    '........\n' +
                    '........\n' +
                    'B\n';
 
  var sampleOutput = '........\n' +
                     '........\n' +
                     '....0...\n' +
                     '...BW0..\n' +
                     '..0WB...\n' +
                     '...0....\n' +
                     '........\n' +
                     '........\n' +
                     'B\n';

  describe('game', function() {

    var game = reversiFromString(sampleInput);

    it('should be able to read in board', function() {
      assert.equal(BLACK, game.turn());
    });

    it('there needs to be at least one of the right color', function() {
      assert.deepEqual([], capturableIndices('...BB...','W'));
    });

    it('can describe a simple capture', function() {
      assert.deepEqual([0], capturableIndices('.WB', 'B'));
      assert.deepEqual([2], capturableIndices('BW.', 'B'));
    });
  });

  describe('acceptance test', function() {

    var drive = function(input) {
      var game = reversiFromString(input);
      return game.possibleMoves();
    };

    it('should produce the expected output', function() {
      assert.equal(sampleOutput, drive(sampleInput));
    });
  });
});
