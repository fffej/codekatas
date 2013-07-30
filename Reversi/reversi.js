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
 
    var sampleOutput = '';

    it('should produce the expected output', function() {
      assert.equal(sampleOutput, drive(sampleInput));
    });
  });
});
