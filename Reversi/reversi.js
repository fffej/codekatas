"use strict";

var assert = require('assert');

var WHITE = 1;
var BLACK = 2;

var isCapture = function(s, c) {

  if (-1 === s.indexOf(c)) {
    return -1;
  }

};

var reversiFromString = function(s) {
  var lines = s.split('\n');

  var black = [];
  var white = [];

  for (var i=0;i<8;++i) {
    var row = lines[i];
    for (var j=0;j<8;++j) {
      if (row[j] === 'B') {
        black.push([i,j]);
      } else if (row[j] === 'W') {
        white.push([i,j]);
      }
    }
  }

  return {
    turn: function() {
      return lines[8] === 'B' ? BLACK : WHITE;
    },

    onBlack: function(f) {
      black.forEach(f);
    },

    onWhite: function(f) {
      white.forEach(f);
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

    it('should be able to visit black squares', function() {
      var b = 0;
      game.onBlack(function() { b++; });
      assert.equal(2, b, "visited two black tokens");
    });

    it('should be able to visit white squares', function() {
      var w = 0;
      game.onWhite(function() { w++; });
      assert.equal(2, w, "visited two white tokens");
    });

    it('there needs to be at least one of the right color', function() {
      assert.equal(-1, isCapture('...BB...','W'));
    });

    it('can describe a simple capture', function() {
      assert.equal(0, isCapture('.WB', 'B'));
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
