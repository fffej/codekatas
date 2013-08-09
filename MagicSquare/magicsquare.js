"use strict";

var assert = require('assert');

var MagicSquare = function(size) {
  return {
    shuffle: function() {}
  };
};

describe('magic square', function() {
  it('exists', function() {
    assert(new MagicSquare(3));
  });

  it('can be shuffled', function() {
    assert(new MagicSquare().shuffle);
  });

  it('can be display', function() {
    assert.equal('123\n456\n78*\n',new MagicSquare().display());
  });
});