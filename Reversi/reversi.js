"use strict";

var assert = require('assert');

var WHITE = 1;
var BLACK = 2;

var reversiFromString = function(s) {

};

var Reversi = function() {
  return {
    width: 8,
    height: 8,
    turn: function() {
      return WHITE;
    }
  };
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
    it('should be able to read in board', function() {
      var game = reversiFromString(sampleInput);

      assert.equal(BLACK, game.turn());
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
