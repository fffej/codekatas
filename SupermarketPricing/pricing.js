"use strict";

var assert = require('assert');

var Item = function(desc) {
  this.description = desc;
  return this;
};

var Basket = function() {
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

  describe('basket', function() {
    it('exists', function() {
      assert(new Basket());
    });

    it('items can be added', function() {
      var basket = new Basket();
      basket.add(new Item('Shoes'));

      assert.equal(1, basket.empty().length);
    });
  });
});
