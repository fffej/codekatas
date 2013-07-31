"use strict";
var assert = require('assert');



describe('reversi', function() {

  it('basic predicates', function() {
    assert(isBlack('B'));
    assert(isWhite('W'));
    assert(isEmpty('.'));
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
