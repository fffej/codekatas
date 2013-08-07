"use strict";

var assert = require('assert');

var White = 0;

var Cell = function() {
  return {
    value: function() { return White; }
  };
}

describe('langtons ant', function() {
  it('has cells', function() {
    assert(new Cell());
  });


  it('cells start white', function() {
    assert.equal(White, new Cell().value());
  });

});