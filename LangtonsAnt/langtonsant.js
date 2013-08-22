"use strict";
var assert = require('assert');

var Plane = function() {
  return this;
};

var Ant = function() {
  return this;
};

describe('infinite grid', function() {
  describe('grid', function() {
    it('exists', function() {
      assert(new Plane());
    });
  });

  describe('ant', function() {
    it('exists', function() {
      assert(new Ant());
    });

    it('has a position', function() {
      assert(new Ant().position());
    });
  });

});
