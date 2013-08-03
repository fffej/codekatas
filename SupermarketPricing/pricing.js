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
    it('have a unit price', function() {
      var item = new Item('beans', 45);
      assert.equal(45, item.unitPrice());
    });
  });

  describe('register', function() {
    it('totals the price of items', function() {
      var register = new Register();
      register.price(new Item('beans', 45));
      register.price(new Item('food', 45));

      assert.equal(90, register.total());
    });
  });
});
