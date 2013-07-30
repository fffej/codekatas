"use strict";

var assert = require('assert');

var WHITE = 1;
var BLACK = 2;

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

  describe('game', function() {
    it('should be self-describing', function() {
      var game = new Reversi();

      assert.equal(8, game.width);
      assert.equal(8, game.height);
    });

    it('should have a notion of turn', function() {
      var game = new Reversi();

      assert.equal(WHITE, game.turn());
    });
  });

  describe('acceptance test', function() {

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

    var drive = function(input) {

    };

    it('should produce the expected output', function() {
      assert.equal(sampleOutput, drive(sampleInput));
    });
  });
});
