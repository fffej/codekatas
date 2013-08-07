"use strict";

var assert = require('assert');

var Cell = function() {
  return {};
}

describe('langtons ant', function() {
  it('has cells', function() {
    assert(new Cell());
  });


  it('cells start white', function() {
    assert.equal(White, new Cell().value());
  });

});