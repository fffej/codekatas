"use strict";

var assert = require('assert');

var Item = function(desc) {
  return this;
};

describe('shopping basket', function() {
  describe('item', function() {
    it('exists', function() {
      assert(new Item('description'));
    });

    it('has a description', function() {
      var item = new Item('banana');
      assert.equal('banana', item.description);
    });
  });
});
