"use strict";

var assert = require('assert');

var WHITE = 1;
var BLACK = 2;

var isBlank = function(c) { return c === '.'; };
var isBlack = function(c) { return c === 'B'; };
var isWhite = function(c) { return c === 'W'; };
var opposite = function(c) { return isBlack(c) ? 'W' : 'B'; };

var capturableIndices = function(s, c) {

  var i = s.indexOf(c);
  if (-1 === i) {
    return -1;
  }

  while (i !== -1) {
    // Is there one after?
    i = s.indexOf(c,i + 1);

    for (var c = i + 1; c < s.length; ++c) {
      if (isBlank(s[c])) {
        return c;
      }
    }
  }  
};

var reversiFromString = function(s) {
  var lines = s.split('\n');

  var black = [];
  var white = [];

  for (var i=0;i<8;++i) {
    var row = lines[i];
    for (var j=0;j<8;++j) {
      if (isBlack(row[j])) {
        black.push([i,j]);
      } else if (isWhite(row[j])) {
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
      assert.equal([], capturableIndices('...BB...','W'));
    });

    it('can describe a simple capture', function() {
      assert.equal(0, capturableIndices('.WB', 'B'));
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
