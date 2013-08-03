"use strict";

var assert = require('assert');

var Item = function(name, price) {
  return {
    unitPrice: function() {
      return price;
    }
  }
};

describe('super market', function() {
  describe('item', function() {
    it('items have a unit price', function() {
      var item = new Item('beans', 45);
      assert.equal(45, item.unitPrice());
    });
  });
});
