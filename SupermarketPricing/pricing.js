"use strict";

var assert = require('assert');

describe('shopping basket', function() {
  describe('item', function() {
    it('exists', function() {
      assert(new Item('description'));
    });
  });
});
