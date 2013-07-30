"use strict";

var assert = require('assert');

describe('reversi', function() {
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
