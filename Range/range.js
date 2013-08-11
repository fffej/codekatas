"use strict";

var assert = require('assert');

var Range = function() {
  
};

describe('range', function() {
  describe('creation', function() {
    it('exists', function() {
      assert(new Range);
    });
  });

  describe('closed intervals', function() {
    it('is created', function() {
      assert(createdCloseRange(1,4));
    });
  });
});
